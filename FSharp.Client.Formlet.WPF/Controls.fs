(* Copyright 1999-2005 The Apache Software Foundation or its licensors, as
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
open System.Collections.ObjectModel
open System.Windows
open System.Windows.Controls

open FSharp.Client.Formlet.Core

open Elements

module internal Controls =

    type BinaryElement () =
        inherit FormletElement ()

        let mutable left                : UIElement         = null
        let mutable right               : UIElement         = null
        let mutable orientation         : FormletOrientation= TopToBottom
        let mutable stretch             : FormletStretch    = NoStretch

        override this.Children =
            match left, right with
                |   null, null  -> [||]
                |   l,null      -> [|l|]
                |   null,r      -> [|r|]
                |   l,r         -> [|l;r;|]


        member this.Orientation
            with get ()                     =   orientation
            and  set (value)                =   orientation <- value
                                                this.InvalidateMeasure ()

        member this.Stretch
            with get ()                     =   stretch
            and  set (value)                =   stretch <- value
                                                this.InvalidateArrange ()

        member this.Left
            with    get ()                  = left
            and     set (fe : UIElement)    =
                if not (Object.ReferenceEquals (left,fe)) then
                    this.RemoveChild (left)
                    left <- fe
                    this.AddChild (left)
                    this.InvalidateMeasure ()

        member this.Right
            with    get ()                  = right
            and     set (fe : UIElement)    =
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

        let label       = CreateLabelTextBox "Label"
        let stackPanel  = CreateVerticalStackPanel ()

        do
            label.Width     <- labelWidth
            this.Orientation<- LeftToRight
            this.Stretch    <- RightStretches
            this.Left       <- label
            this.Right      <- stackPanel

        member this.Text
            with get ()     = label.Text
            and  set value  = label.Text <- value

        member this.ChildCollection = stackPanel.Children

    type InputTextElement(initialText : string) as this =
        inherit TextBox()

        let mutable text                = initialText

        do
            this.Text   <- initialText
            this.Margin <- DefaultMargin

        member val ChangeNotifier = EmptyChangeNotification with get, set

        override this.OnLostFocus(e) =
            base.OnLostFocus(e)

            if text <> this.Text then
                text <- this.Text

                this.ChangeNotifier ()

    type InputDateTimeElement(initialDateTime : DateTime option) as this =
        inherit DatePicker()

        let mutable dateTime= initialDateTime

        let getNullableDateTime (dt : DateTime option) = 
            match dt with
            | Some d    -> Nullable<DateTime> (d)
            | _         -> Nullable<DateTime> ()

        do
            this.Margin         <- DefaultMargin
            this.SelectedDate   <- getNullableDateTime initialDateTime

        member val ChangeNotifier = EmptyChangeNotification with get, set

        member private this.GetDateTime () =
            let d = this.SelectedDate
            if d.HasValue then Some d.Value
            else None

        member this.DateTime 
            with    get () : DateTime option    = dateTime
            and     set (dt : DateTime option)  =
                dateTime            <- dt
                this.SelectedDate   <- getNullableDateTime dt

        override this.OnSelectedDateChanged(e)  = 
            base.OnSelectedDateChanged(e)

            let currentDate = this.GetDateTime ()

            if dateTime <> currentDate then
                this.DateTime <- currentDate 

                this.ChangeNotifier ()

    type ManyElement() as this =
        inherit BinaryElement()

        let listBox, buttons, newButton, deleteButton = CreateManyElements this.CanExecuteNew this.ExecuteNew this.CanExecuteDelete this.ExecuteDelete

        let inner = new ObservableCollection<UIElement> ()

        do
            this.Stretch        <- RightStretches
            listBox.ItemsSource <- inner
            this.Left           <- buttons
            this.Right          <- listBox

        member val ChangeNotifier = EmptyChangeNotification with get, set

        member this.ExecuteNew ()   =   inner.Add null
                                        this.ChangeNotifier ()
        member this.CanExecuteNew ()=   true

        member this.ExecuteDelete ()=   let selectedItems = listBox.SelectedItems
                                        let selection = Array.create selectedItems.Count (null :> UIElement)
                                        for i in 0..selectedItems.Count - 1 do
                                            selection.[i] <- selectedItems.[i] :?> UIElement

                                        for i in selectedItems.Count - 1..-1..0 do
                                            ignore <| inner.Remove(selection.[i])

                                        this.ChangeNotifier ()

        member this.CanExecuteDelete () = listBox.SelectedItems.Count > 0

        member this.ChildCollection = inner


    type LegendElement(outer : UIElement, label : TextBox, inner : Decorator) =
        inherit DecoratorElement(outer)

        let stackPanel  = CreateVerticalStackPanel ()

        do 
            inner.Child <- stackPanel

        new () = 
            let outer, label, inner = CreateLegendElements "Group"
            new LegendElement (outer, label, inner)

        member this.Text
            with get ()                     = label.Text
            and  set (value)                = label.Text <- value

        member this.ChildCollection = stackPanel.Children

