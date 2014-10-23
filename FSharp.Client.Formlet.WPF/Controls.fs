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
open System.Collections
open System.Collections.Generic
open System.Collections.ObjectModel
open System.Windows
open System.Windows.Controls
open System.Windows.Documents
open System.Windows.Input
open System.Windows.Media

open FSharp.Client.Formlet.Core

open Elements

module internal Controls =

    type SymbolElement(layers : (string*double*Brush*Typeface) array) =
        inherit FrameworkElement()

        let formattedTexts =
            layers
            |> Array.map (fun (text,size, brush, typeFace) -> FormatText text typeFace size brush)


        override this.MeasureOverride(sz : Size) =
            ignore <| base.MeasureOverride sz
            let mutable s = Size ()
            for formattedText in formattedTexts do
                let is = Size (formattedText.Width, formattedText.Height)
                s <- Union s is

            s

        override this.OnRender (drawingContext) =
            let rs = this.DesiredSize
            for formattedText in formattedTexts do
                let p = Point ((rs.Width - formattedText.Width) / 2.0, (rs.Height - formattedText.Height) / 2.0)
                drawingContext.DrawText (formattedText, p)

    type ContainerElement () as this =
        inherit FormletElement ()

        let mutable vertical            = true
        let mutable expandLast          = true
        let mutable duplicateDetector   = HashSet<UIElement> ()
        let mutable existing            = HashSet<UIElement> ()

        let children                    = ResizeArray<UIElement>()

        let invalidate () =
            this.InvalidateMeasure ()

        let get i =
            if i < children.Count then children.[i]
            else null

        interface IList<UIElement> with
            member this.Count       = children.Count
            member this.IsReadOnly  = false
            member this.Add e       =
                children.Add e
                this.AddChild e
                invalidate ()
            member this.Clear ()    =
                let count = children.Count
                for i in 0..count - 1 do
                    let child = children.[i]
                    this.RemoveChild child
                children.Clear ()
                invalidate ()
            member this.Contains e  = children.Contains e
            member this.CopyTo (i,a)= children.CopyTo (i,a)
            member this.IndexOf e   = children.IndexOf e
            member this.Insert (i,e)=
                let existing = get i
                if not (Object.ReferenceEquals (existing, e)) then
                    children.Insert (i,e)
                    this.RemoveChild existing
                    this.AddChild e
                    invalidate ()
            member this.Remove e    =
                let res = children.Remove e
                if res then
                    this.RemoveChild e
                    invalidate ()
                res
            member this.RemoveAt i  =
                let existing = get i
                children.RemoveAt i
                this.RemoveChild existing
                invalidate ()

            member this.Item
                with get(index)         = children.[index]
                and  set(index)(e)  =
                    let existing = get index
                    if not (Object.ReferenceEquals (existing, e)) then
                        children.[index] <- e
                        this.RemoveChild existing
                        this.AddChild e
                        invalidate ()

            member this.GetEnumerator ()    = children.GetEnumerator () :> IEnumerator<UIElement>
            member this.GetEnumerator()     = children.GetEnumerator () :> IEnumerator

        member x.Orientation
            with get ()     =
                if vertical then TopToBottom
                else LeftToRight
            and  set o      =
                match vertical, o with
                | false, TopToBottom
                | true , LeftToRight    -> vertical <- o = TopToBottom; invalidate ()
                | _                     -> ()

        member x.ExpandLast
            with get ()     = expandLast
            and  set e      =
                match expandLast, e with
                | false, true
                | true , false      -> vertical <- e; invalidate ()
                | _    , _          -> ()


        override this.LogicalChildren = children.GetEnumerator () :> IEnumerator

        override this.VisualChildrenCount = children.Count

        override this.GetVisualChild (i : int) = upcast children.[i]

        override this.MeasureOverride (sz : Size) =
            let sz = this.ModifyMeasure sz

            ignore <| base.MeasureOverride sz

            let mutable estimated = EmptySize
            let mutable remaining = sz

            let count = children.Count
            for i in 0..count - 1 do
                let child = children.[i]
                if child <> null then
                    child.Measure remaining
                    let desired = child.DesiredSize
                    remaining <- ExceptSize (vertical, remaining, desired)
                    estimated <- UnionSize (vertical, estimated, desired)

            Intersect sz estimated

        override this.ArrangeOverride (sz : Size) =
            ignore <| base.ArrangeOverride sz

            let mutable arranged = this.ModifyArrange EmptyRect

            let count = children.Count
            for i in 0..count - 1 do
                let child = children.[i]
                if child <> null then
                    let expand = (i = count - 1) && expandLast
                    arranged <- Arrange (vertical, expand, sz, arranged, child.DesiredSize)
                    child.Arrange arranged

            sz

        abstract ModifyMeasure : Size -> Size
        abstract ModifyArrange : Rect -> Rect

        default this.ModifyMeasure sz   = sz
        default this.ModifyArrange r    = r

        member this.ChildCollection = this :> IList<UIElement>

    // TODO: Make label text selectable, using TextBox is an easy way to do this.
    type LabelElement (labelWidth : double) as this =
        inherit ContainerElement ()

        let mutable formattedText   = FormatText "Label" DefaultTypeFace 12. DefaultForegroundBrush
        let origin = Point(2.,4.)

        do
            this.Orientation <- FormletOrientation.LeftToRight

        member this.Text
            with get ()     = formattedText.Text
            and  set value  =
                if formattedText.Text <> value then
                    formattedText <- FormatText value DefaultTypeFace 12. DefaultForegroundBrush
                    this.InvalidateVisual ()

        override this.OnRender (dc : DrawingContext) =
            dc.DrawText (formattedText, origin)
            ()

        override this.ModifyMeasure sz   =
            let desired     = Size (labelWidth, sz.Height)
            ExceptHorizontally (sz, desired)

        override this.ModifyArrange r    =  Rect (r.X + labelWidth, r.Y, r.Width, r.Height)

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

    type ManyElement(value : StackPanel) as this =
        inherit DecoratorElement(value)

        let inner = ObservableCollection<UIElement> ()
        let listBox, buttons, newButton, deleteButton = CreateManyElements this.CanExecuteNew this.ExecuteNew this.CanExecuteDelete this.ExecuteDelete

        do
            listBox.ItemsSource <- inner
            ignore <| value.Children.Add buttons
            ignore <| value.Children.Add listBox

        new () =
            new ManyElement (CreateStackPanel Orientation.Vertical)

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

        let container   = new ContainerElement ()

        do
            inner.Child <- container

        new () =
            let outer, label, inner = CreateLegendElements "Group"
            new LegendElement (outer, label, inner)

        member this.Text
            with get ()                     = label.Text
            and  set (value)                = label.Text <- value

        member this.ChildCollection = container :> IList<UIElement>

    type ErrorSummaryElement(sp : StackPanel) as this =
        inherit DecoratorElement(sp)

        static let okBackgroundBrush    = CreateSimpleGradient (CreateColor "#0B0") (CreateColor "#070") :> Brush
        static let errorBackgroundBrush = CreateSimpleGradient (CreateColor "#B00") (CreateColor "#700") :> Brush

        static let okBorderBrush        = CreateBrush <| CreateColor "#070"
        static let errorBorderBrush     = CreateBrush <| CreateColor "#705"

        static let okSymbolBackgroundBrush      = CreateSimpleGradient (CreateColor "#0F0") (CreateColor "#0B0") :> Brush
        static let errorSymbolBackgroundBrush   = CreateSimpleGradient (CreateColor "#F00") (CreateColor "#B00") :> Brush

        let symbolSize  = 48.0
        let largeSize   = 24.0

        let container   = new ContainerElement ()
        let border      = new Border ()
        let grid        = new Grid ()
        let stackPanel  = CreateStackPanel Orientation.Vertical

        let submitButton= CreateButton "_Submit" "Click to submit form" this.CanSubmit this.Submit
        let resetButton = CreateButton "_Reset" "Click to reset form"   this.CanReset this.Reset

        let errorSymbol = new SymbolElement (   [|
                                                    ("\u26CA", symbolSize       , errorSymbolBackgroundBrush, SymbolTypeFace    )
                                                    ("\u26C9", symbolSize       , DefaultBackgroundBrush    , SymbolTypeFace    )
                                                    ("\u2757", symbolSize / 2.0 , DefaultBackgroundBrush    , SymbolTypeFace    )
                                                |]
                                                )
        let okSymbol    = new SymbolElement (   [|
                                                    ("\u26CA", symbolSize       , okSymbolBackgroundBrush   , SymbolTypeFace    )
                                                    ("\u26C9", symbolSize       , DefaultBackgroundBrush    , SymbolTypeFace    )
                                                    ("\u2714", symbolSize / 2.0 , DefaultBackgroundBrush    , SymbolTypeFace    )
                                                |])
        let label = CreateTextBlock ""

        let mutable failures = []

        do
            stackPanel.Margin       <- Thickness (0.0, 9.0, 0.0, 0.0)
            label.Foreground        <- DefaultBackgroundBrush
            label.FontFamily        <- DefaultFontFamily

            ignore <| stackPanel.Children.Add submitButton
            ignore <| stackPanel.Children.Add resetButton

            grid
            |>  AddGridColumn_Star          1.0
            |>  AddGridColumn_Pixel         4.0
            |>  AddGridColumn_Auto
            |>  AddGridColumn_Pixel         4.0
            |>  AddGridColumn_Auto
            |>  AddGridColumn_Pixel         8.0
            |>  AddGridChild label          0   0
            |>  AddGridChild okSymbol       4   0
            |>  AddGridChild errorSymbol    4   0
            |>  AddGridChild stackPanel     2   0
            |>  ignore


            border.Background       <- okBackgroundBrush
            border.BorderBrush      <- okBorderBrush
            border.BorderThickness  <- Thickness 2.0
            border.Margin           <- DefaultMargin
            border.CornerRadius     <- CornerRadius 8.0
            border.Padding          <- DefaultMargin
            border.Child            <- grid

            ignore <| sp.Children.Add border
            ignore <| sp.Children.Add container

        new () =
            let sp = CreateStackPanel Orientation.Vertical
            ErrorSummaryElement sp


        member this.Failures
            with get ()                             = failures
            and  set (value : FormletFailure list)  =
                failures <- value |> List.rev
                CommandManager.InvalidateRequerySuggested()
                label.Inlines.Clear ()
                let inlines =
                    if not failures.IsEmpty then
                        errorSymbol.Visibility  <- Visibility.Visible
                        okSymbol.Visibility     <- Visibility.Collapsed
                        border.Background       <- errorBackgroundBrush
                        border.BorderBrush      <- errorBorderBrush
                        let inlines =
                            failures
                            |>  List.collect (fun f ->
                                [
                                    new Run (" § ")             :> Inline
                                    new Run (f.FailureContext   |> LastOrDefault "No context"   )
                                                                :> Inline
                                    new Run (" - ")             :> Inline
                                    new Run (f.Message)         :> Inline
                                    new LineBreak()             :> Inline
                                ])
                        let full =
                            [
                                new Run ("You can't submit because")    |?> (fun r -> r.FontSize <- largeSize)
                                                                        :> Inline
                                new LineBreak()                         :> Inline
                            ]
                            @ inlines
                        full |>  List.toArray
                    else
                        errorSymbol.Visibility  <- Visibility.Collapsed
                        okSymbol.Visibility     <- Visibility.Visible
                        border.Background       <- okBackgroundBrush
                        border.BorderBrush      <- okBorderBrush
                        [|
                                new Run ("Ready to submit")     |?> (fun r -> r.FontSize <- largeSize)
                                                                :> Inline
                                new LineBreak()                 :> Inline
                        |]
                label.Inlines.AddRange (inlines)
                if label.Inlines.Count = 0 then
                    label.Visibility <- Visibility.Collapsed
                else
                    label.Visibility <- Visibility.Visible

        member this.Submit ()   = FormletElement.RaiseSubmit this
        member this.CanSubmit ()= this.Failures.IsEmpty

        member this.Reset ()    = FormletElement.RaiseReset this
        member this.CanReset () = true

        member this.ChildCollection = container.ChildCollection
