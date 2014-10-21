﻿(* Copyright 1999-2005 The Apache Software Foundation or its licensors, as
 * applicable.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

namespace FSharp.Client.Formlet.WPF

open System
open System.Collections
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Windows
open System.Windows.Controls
open System.Windows.Threading

open FSharp.Client.Formlet.Core

module Controls =

    type FormletDispatchAction =
        | Rebuild   = 1
        | Submit    = 2
        | Reset     = 3

    [<AbstractClass>]
    type FormletElement () as this =
        inherit FrameworkElement ()

        let mutable isInitialized = false

        do
            this.Margin <- DefaultMargin

        static let rebuildEvent     = CreateRoutedEvent<FormletElement> "Rebuild"
        static let submitEvent      = CreateRoutedEvent<FormletElement> "Submit"
        static let resetEvent       = CreateRoutedEvent<FormletElement> "Reset"

        static member RebuildEvent  = rebuildEvent
        static member SubmitEvent   = submitEvent
        static member ResetEvent    = resetEvent

        static member RaiseRebuild (sender : UIElement) = RaiseRoutedEvent FormletElement.RebuildEvent  sender
        static member RaiseSubmit  (sender : UIElement) = RaiseRoutedEvent FormletElement.SubmitEvent   sender
        static member RaiseReset   (sender : UIElement) = RaiseRoutedEvent FormletElement.ResetEvent    sender

        override this.MeasureOverride (sz : Size) =
            if not isInitialized then
                isInitialized <- true
                this.OnStartUp ()
                FormletElement.RaiseRebuild this

            base.MeasureOverride sz

        abstract member OnStartUp : unit -> unit

        default this.OnStartUp () = ()

        abstract Children : array<UIElement> with get

        override this.LogicalChildren = this.Children |> Enumerator

        override this.VisualChildrenCount = this.Children.Length

        override this.GetVisualChild (i : int) = upcast this.Children.[i]

        member this.RemoveChild (fe : UIElement) =
                this.RemoveVisualChild (fe)
                this.RemoveLogicalChild (fe)

        member this.AddChild (fe : UIElement) =
                this.AddLogicalChild (fe)
                this.AddVisualChild (fe)

        member this.Rebuild () = FormletElement.RaiseRebuild this

    [<AbstractClass>]
    type DecoratorElement (value : UIElement) as this =
        inherit FormletElement ()

        let children = [|value|]

        do 
            this.AddChild value

        override this.Children
            with    get ()   = children

        override this.MeasureOverride (sz : Size) =
            ignore <| base.MeasureOverride sz
            value.Measure (sz)
            value.DesiredSize

        override this.ArrangeOverride (sz : Size) =
            ignore <| base.ArrangeOverride sz
            value.Arrange (Rect (sz))
            sz

    type BinaryElement () =
        inherit FormletElement ()

        let mutable left                : UIElement         = null
        let mutable right               : UIElement         = null
        let mutable orientation         : FormletOrientation= TopToBottom
        let mutable stretch             : FormletStretch    = NoStretch

        override this.Children
            with    get ()   =
                match left, right with
                    |   null, null  -> [||]
                    |   l,null      -> [|l|]
                    |   null,r      -> [|r|]
                    |   l,r         -> [|l;r;|]


        member this.Orientation
            with get ()                         =   orientation
            and  set (value)                    =   orientation <- value
                                                    this.InvalidateMeasure ()

        member this.Stretch
            with get ()                         =   stretch
            and  set (value)                    =   stretch <- value
                                                    this.InvalidateArrange ()

        member this.Left
            with    get ()                      = left
            and     set (fe : UIElement) =
                if not (Object.ReferenceEquals (left,fe)) then
                    this.RemoveChild (left)
                    left <- fe
                    this.AddChild (left)
                    this.InvalidateMeasure ()

        member this.Right
            with    get ()                      = right
            and     set (fe : UIElement)  =
                if not (Object.ReferenceEquals (right,fe)) then
                    this.RemoveChild (right)
                    right <- fe
                    this.AddChild (right)
                    this.InvalidateMeasure ()

        override this.LogicalChildren = this.Children |> Enumerator

        override this.VisualChildrenCount = this.Children.Length

        override this.GetVisualChild (i : int) = upcast this.Children.[i]

        override this.MeasureOverride (sz : Size) =
            ignore <| base.MeasureOverride sz
            let c = this.Children
            match c with
                |   [||]    ->  EmptySize
                |   [|v|]   ->  v.Measure (sz)
                                v.DesiredSize
                |   [|l;r;|]->  l.Measure (sz)
                                let nsz = ExceptUsingOrientation orientation sz l.DesiredSize
                                r.Measure (nsz)
                                let result = Intersect sz (UnionUsingOrientation orientation l.DesiredSize r.DesiredSize)
                                result
                |   _       ->  HardFail_InvalidCase ()

        override this.ArrangeOverride (sz : Size) =
            ignore <| base.ArrangeOverride sz
            let c = this.Children
            match c with
                |   [||]    ->  ()
                |   [|v|]   ->  let r = TranslateUsingOrientation orientation false sz EmptyRect v.DesiredSize
                                ignore <| v.Arrange (r)
                |   [|l;r;|]->  let fillRight = stretch = RightStretches
                                let lr = TranslateUsingOrientation orientation false sz EmptyRect l.DesiredSize
                                let rr = TranslateUsingOrientation orientation fillRight sz lr r.DesiredSize
                                l.Arrange (lr)
                                r.Arrange (rr)
                                ignore <| r.Arrange (rr)
                |   _       ->  HardFail_InvalidCase ()

            sz

    type LabelElement (labelWidth : double) as this =
        inherit BinaryElement ()

        let label = CreateLabelTextBox "Label"

        do
            label.Width     <- labelWidth
            this.Orientation<- LeftToRight
            this.Stretch    <- RightStretches
            this.Left       <- label

        member this.Text
            with get ()     = label.Text
            and  set value  = label.Text <- value

    type InputTextElement(initialText : string) as this =
        inherit TextBox()

        let mutable text        = initialText
        let mutable cacheChain  = []

        do
            this.Text   <- initialText
            this.Margin <- DefaultMargin

        member x.CacheChain
            with    get () : IFormletCache list = cacheChain
            and     set (cc : IFormletCache list) = cacheChain <- cc

        override this.OnLostFocus(e) =
            base.OnLostFocus(e)

            if text <> this.Text then
                text <- this.Text

                for c in cacheChain do
                    c.Clear ()

                FormletElement.RaiseRebuild this

    type ManyElement(initialCount : int) as this =
        inherit BinaryElement()

        let listBox, buttons, newButton, deleteButton = CreateManyElements this.CanExecuteNew this.ExecuteNew this.CanExecuteDelete this.ExecuteDelete

        let inner = new ObservableCollection<UIElement> ()

        do
            for i in 0..initialCount - 1 do
                inner.Add null
            this.Stretch        <- RightStretches
            listBox.ItemsSource <- inner
            this.Left           <- buttons
            this.Right          <- listBox

            FormletElement.RaiseRebuild this

        member this.ExecuteNew ()   =   inner.Add null
                                        FormletElement.RaiseRebuild this
        member this.CanExecuteNew ()=   true

        member this.ExecuteDelete ()=   let selectedItems = listBox.SelectedItems
                                        let selection = Array.create selectedItems.Count (null :> UIElement)
                                        for i in 0..selectedItems.Count - 1 do
                                            selection.[i] <- selectedItems.[i] :?> UIElement

                                        for i in selectedItems.Count - 1..-1..0 do
                                            ignore <| inner.Remove(selection.[i])
                                            FormletElement.RaiseRebuild this

        member this.CanExecuteDelete () = listBox.SelectedItems.Count > 0

        member this.Inner with get ()   = inner


    type LegendElement(outer : UIElement, label : TextBox, inner : Decorator) =
        inherit DecoratorElement(outer)

        new () = 
            let outer, label, inner = CreateLegendElements "Group"
            new LegendElement (outer, label, inner)

        member this.Inner
            with get ()                     = inner.Child
            and  set (value : UIElement)    = inner.Child <- value

        member this.Text
            with get ()                     = label.Text
            and  set (value)                = label.Text <- value

    type FormletContext () =
        interface IFormletContext with
            member x.PushTag tag            = ()
            member x.PopTag ()              = ()
            member x.PushLabelWidth width   = ()
            member x.PopLabelWidth ()       = ()
            member x.LabelWidth             = 100.

    type FormletControl<'TValue> (scrollViewer : ScrollViewer, submit : 'TValue -> unit, formlet : Formlet<FormletContext, UIElement, 'TValue>) as this =
        inherit DecoratorElement (scrollViewer)

        let queue                       = SingleDispatchQueue<FormletDispatchAction> (this.Dispatcher)
        let mutable formTree            = Empty
        let mutable changeGeneration    = 0

        do
            AddRoutedEventHandler FormletElement.RebuildEvent  this this.OnRebuild
            AddRoutedEventHandler FormletElement.SubmitEvent   this this.OnSubmit
            AddRoutedEventHandler FormletElement.ResetEvent    this this.OnReset

            scrollViewer.HorizontalScrollBarVisibility  <- ScrollBarVisibility.Disabled
            scrollViewer.VerticalScrollBarVisibility    <- ScrollBarVisibility.Visible

        let layout = FormletLayout.New TopToBottom Maximize Maximize

        let setElement (collection : IList) (position : int) (element : UIElement) : unit =
            if position < collection.Count then
                collection.[position] <- element
            else if position = collection.Count then
                ignore <| collection.Add element
            else
                HardFail_InvalidCase ()

        let getElement (collection : IList) (position : int) : UIElement =
            if position < collection.Count then
                match collection.[position] with
                | :? UIElement as e -> e
                | _                 -> null
            else null
        let rec buildTree (collection : IList) (position : int) (fl : FormletLayout) (ft : FormletTree<UIElement>) : int =
            let current = getElement collection position

            match ft with
            | Empty                 ->
                0
            | Element e           ->
                setElement collection position e
                1
            | Adorner (e, ls, fts) ->
                fts |> List.iteri (fun i v -> ignore <| buildTree collection i fl v)
                setElement collection position e
                1
            | Layout (l, ft)        ->
                let nl = fl.Union l
                if nl = fl then
                    buildTree collection position fl ft
                else
                    let sp = CreateElement current CreateVerticalStackPanel
                    sp.Orientation <-
                        match fl.Orientation with
                        | FormletOrientation.Parent
                        | FormletOrientation.TopToBottom   -> Orientation.Vertical
                        | FormletOrientation.LeftToRight   -> Orientation.Horizontal

                    ignore <| buildTree sp.Children 0 fl ft
                    setElement collection position sp
                    1
            | Label (lbl, ft)       ->
                let label = CreateElement current (fun () -> LabelElement (100.))
                label.Text  <- lbl
                // label.Right <- null // TODO:
                let sp = CreateElement label.Right CreateVerticalStackPanel
                ignore <| buildTree sp.Children 0 fl ft
                label.Right <- sp
                setElement collection position label
                1
            | Fork (l,r)            ->
                let lcount = buildTree collection position fl l
                let rcount = buildTree collection (position + lcount) fl r
                lcount + rcount
            | Modify (modifier, ft)     ->
                let c       = buildTree collection position fl ft
                let element = getElement collection position
                modifier element
                c   // TODO: Should map be applied to last, first or all?
            | Group (_, ft)         ->
                buildTree collection position fl ft
            | Tag (_, ft)           ->
                buildTree collection position fl ft
            | Cache (_, ft)         ->
                buildTree collection position fl ft

        new (submit : 'TValue -> unit, formlet : Formlet<FormletContext, UIElement, 'TValue>) = 
            let scrollViewer = new ScrollViewer ()
            FormletControl (scrollViewer, submit, formlet)

        member this.OnRebuild   (sender : obj) (e : RoutedEventArgs) = queue.Dispatch (FormletDispatchAction.Rebuild  , this.BuildForm)
        member this.OnSubmit    (sender : obj) (e : RoutedEventArgs) = queue.Dispatch (FormletDispatchAction.Submit   , this.SubmitForm)
        member this.OnReset     (sender : obj) (e : RoutedEventArgs) = queue.Dispatch (FormletDispatchAction.Reset    , this.ResetForm)

        override this.OnStartUp () =
            this.BuildForm ()

        member this.ResetForm () =
            scrollViewer.Content <- null
            this.BuildForm ()

        member this.Evaluate () =
            let context = FormletContext ()
            let c,ft = formlet.Evaluate (context, [], formTree)
            formTree <- ft
            c,ft

        member this.SubmitForm () =
            let c,_ = this.Evaluate ()

            if not c.HasFailures then
                submit c.Value

        member this.BuildForm () =
            let _,ft= this.Evaluate ()
            let cft = FormletTree.Layout (layout, ft)
            let sp  = CreateElement scrollViewer.Content CreateVerticalStackPanel

            // TODO: Defer updates

            ignore <| buildTree sp.Children 0 layout cft
            scrollViewer.Content <- sp

            ()

