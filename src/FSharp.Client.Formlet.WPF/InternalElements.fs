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

module internal InternalElements =

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

    type LayoutElement () as this =
        inherit FormletElement ()

        let mutable vertical            = true
        let mutable expandLast          = true

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
                with get(index)     = children.[index]
                and  set(index)(e)  =
                    let existing = get index
                    if not (Object.ReferenceEquals (existing, e)) then
                        children.[index] <- e
                        this.RemoveChild existing
                        this.AddChild e
                        invalidate ()

            member this.GetEnumerator ()    = children.GetEnumerator () :> IEnumerator<UIElement>
            member this.GetEnumerator()     = children.GetEnumerator () :> IEnumerator

        member this.Orientation
            with get ()     =
                if vertical then TopToBottom
                else LeftToRight
            and  set o      =
                match vertical, o with
                | false, TopToBottom
                | true , LeftToRight    -> vertical <- o = TopToBottom; invalidate ()
                | _                     -> ()

        member this.ExpandLast
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
        inherit LayoutElement ()

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

    type InputTriStateElement(initial : bool option) as this =
        inherit CheckBox()

        let mutable triState= initial

        let update () =
            let isChecked = ToOption this.IsChecked

            if triState <> isChecked then
                this.TriState <- isChecked

                this.ChangeNotifier ()

        do
            this.IsChecked  <- ToNullable initial
            this.Margin     <- DefaultMargin

        member val ChangeNotifier = EmptyChangeNotification with get, set

        member this.TriState
            with    get () : bool option    = triState
            and     set (ts : bool option)  =
                triState         <- ts
                this.IsChecked   <- ToNullable ts

        override this.OnChecked(e) =
            base.OnChecked(e)
            update ()

        override this.OnUnchecked(e) =
            base.OnUnchecked(e)
            update ()

    type InputDateTimeElement(initialDateTime : DateTime option) as this =
        inherit DatePicker()

        let mutable dateTime= initialDateTime

        do
            this.Margin         <- DefaultMargin
            this.SelectedDate   <- ToNullable initialDateTime

        member val ChangeNotifier = EmptyChangeNotification with get, set

        member private this.GetDateTime () =
            let d = this.SelectedDate
            if d.HasValue then Some d.Value
            else None

        member this.DateTime
            with    get () : DateTime option    = dateTime
            and     set (dt : DateTime option)  =
                dateTime            <- dt
                this.SelectedDate   <- ToNullable dt

        override this.OnSelectedDateChanged(e)  =
            base.OnSelectedDateChanged(e)

            let currentDate = this.GetDateTime ()

            if dateTime <> currentDate then
                this.DateTime <- currentDate

                this.ChangeNotifier ()

    type InputOptionElement<'T>(initial : int, options : (string * 'T) []) as this =
        inherit ComboBox()

        let itemSource              = ObservableCollection<ComboBoxItem> ()
        let mutable selectedIndex   = -1

        do
            this.Margin         <- DefaultMargin
            this.ItemsSource    <- itemSource

            for i in 0..options.Length - 1 do
                let t,_     = options.[i]
                let tb      = TextBlock ()
                tb.Text     <- t
                let cbi     = ComboBoxItem ()
                cbi.Content <- tb
                itemSource.Add (cbi)

            selectedIndex       <- min initial (options.Length - 1)
            this.SelectedIndex  <- selectedIndex

        member val ChangeNotifier = EmptyChangeNotification with get, set

        member this.SelectedOption: 'T option =
            let i           = this.SelectedIndex
            let hasValue    = i > - 1 && i < options.Length
            if hasValue then
                let _,v = options.[i]
                Some v
            else None

        override this.OnSelectionChanged(e) =
            base.OnSelectionChanged(e)
            let i = this.SelectedIndex
            if selectedIndex <> i then
                selectedIndex <- i

                this.ChangeNotifier ()

    type FormletListBoxItem () as this =
        inherit ListBoxItem ()

        static let pen      = CreatePen DefaultBorderBrush 1.0
        static let typeFace = DefaultTypeFace

        static let transform =
            let transform = Matrix.Identity
            transform.Rotate 90.0
            transform.Translate (DefaultListBoxItemPadding.Left + 5.0, 4.0)
            MatrixTransform (transform)

        let mutable formattedText = Unchecked.defaultof<FormattedText>
        let mutable lastIndex = -1

        do
            this.HorizontalContentAlignment <- HorizontalAlignment.Stretch
            this.Padding <- DefaultListBoxItemPadding

        override this.OnPropertyChanged (e) =
            base.OnPropertyChanged e
            if e.Property = ListBox.AlternationIndexProperty then
                this.InvalidateVisual ()

        override this.OnRender (drawingContext) =

            let index = ListBox.GetAlternationIndex (this)
            if index <> lastIndex || formattedText = null then
                let text  = (index + 1).ToString("000", DefaultCulture)
                formattedText <- FormatText
                    text
                    typeFace
                    24.0
                    DefaultBackgroundBrush
                lastIndex <- index

            let rs = this.RenderSize

            let rect = Rect (0.0, 0.0, this.Padding.Left, rs.Height)

            drawingContext.DrawRectangle (DefaultBorderBrush, null, rect)

            let p0 = Point (0.0, rs.Height)
            let p1 = Point (rs.Width, rs.Height)
            drawingContext.DrawLine (pen, p0, p1)

            drawingContext.PushTransform transform

            drawingContext.DrawText (formattedText, Point (0.0, 0.0))

            drawingContext.Pop ()

    type FormletElement () =
        inherit LayoutElement ()

        member val FormletTree : FormletTree<UIElement> = Empty with get,set

    type AdornersAdapter(adorners : ObservableCollection<FormletElement>) =

        let enumerator : seq<IList<UIElement>*FormletTree<UIElement>> =
            seq {
                for adorner in adorners do
                    yield adorner.ChildCollection, adorner.FormletTree
            }

        interface IReadOnlyList<IList<UIElement>*FormletTree<UIElement>> with
            member this.Count       = adorners.Count

            member this.Item
                with get(index)     =
                    let adorner = adorners.[index]
                    adorner.ChildCollection, adorner.FormletTree

            member this.GetEnumerator ()    = enumerator.GetEnumerator ()
            member this.GetEnumerator()     = enumerator.GetEnumerator () :> IEnumerator

        member this.SetFormletTree (i, ft)     =
            let adorner = adorners.[i]
            adorner.FormletTree <- ft

    type FormletListBox (initialCount : int) as this =
        inherit ListBox ()

        let adorners    = ObservableCollection<FormletElement> ()
        let adapter     = AdornersAdapter adorners

        let addNew ()   = adorners.Add (FormletElement ())

        do
            this.AlternationCount   <- Int32.MaxValue
            this.ItemsSource        <- adorners
            for i = 0 to initialCount - 1 do
                addNew ()

        override this.GetContainerForItemOverride () = FormletListBoxItem () :> DependencyObject

        member val ChangeNotifier = EmptyChangeNotification with get, set

        member this.Adorners                = adapter

        member this.CanAddNew ()            = true
        member this.AddNew ()               =
            addNew ()
            this.ChangeNotifier ()

        member this.CanDeleteSelection ()   = this.SelectedItems.Count > 0
        member this.DeleteSelection ()      =
            let selectedItems = this.SelectedItems
            let selection = Array.zeroCreate selectedItems.Count
            for i = 0 to selectedItems.Count - 1 do
                selection.[i] <- selectedItems.IndexOf selectedItems.[i]

            Array.Sort selection

            for i = selection.Length - 1 downto 0 do
                ignore <| adorners.RemoveAt (selection.[i])

            this.ChangeNotifier ()

    let CreateFormletListBox initialCount =
        let listBox             = FormletListBox initialCount
        listBox.Margin          <- DefaultMargin
        listBox.SelectionMode   <- SelectionMode.Extended
        listBox.MinHeight       <- 24.0
        listBox.MaxHeight       <- 240.0
        ScrollViewer.SetVerticalScrollBarVisibility(listBox, ScrollBarVisibility.Visible)
        ScrollViewer.SetHorizontalScrollBarVisibility(listBox, ScrollBarVisibility.Disabled)
        listBox

    let CreateManyElements initialCount : FormletListBox*IReadOnlyList<IList<UIElement>*FormletTree<UIElement>>*Panel =
        let listBox         = CreateFormletListBox initialCount

        let buttons         = CreateStackPanel Orientation.Horizontal
        // TODO: Localization
        let newButton       = CreateButton "_New" "Click to create another item" listBox.CanAddNew listBox.AddNew
        let deleteButton    = CreateButton "_Delete" "Click to delete the currently selected items" listBox.CanDeleteSelection listBox.DeleteSelection
        ignore <| buttons.Children.Add newButton
        ignore <| buttons.Children.Add deleteButton
        listBox, upcast listBox.Adorners, upcast buttons

    type ManyElement(initialCount : int, value : StackPanel) =
        inherit DecoratorElement(value)

        let listBox, adorners, buttons = CreateManyElements initialCount

        do
            ignore <| value.Children.Add buttons
            ignore <| value.Children.Add listBox

        new (initialCount : int) =
            ManyElement (initialCount, CreateStackPanel Orientation.Vertical)

        member this.ChildCollection = adorners

        member this.ChangeNotifier
            with get () = listBox.ChangeNotifier
            and  set c  = listBox.ChangeNotifier <- c

    let CreateLegendElements t : UIElement*TextBox*Decorator =
        let label               = CreateLabelTextBox t
        label.Background        <- DefaultBackgroundBrush
        label.RenderTransform   <- TranslateTransform (8.0, -6.0)
        label.FontSize          <- 16.0
        let border              = Border ()
        let outer               = Grid ()
        border.Margin           <- DefaultBorderMargin
        border.Padding          <- DefaultBorderPadding
        border.BorderThickness  <- DefaultBorderThickness
        border.BorderBrush      <- DefaultBorderBrush
        ignore <| outer.Children.Add(border)
        ignore <| outer.Children.Add(label)
        upcast outer, label, upcast border

    type LegendElement(outer : UIElement, label : TextBox, inner : Decorator) =
        inherit DecoratorElement(outer)

        let container   = LayoutElement ()

        do
            inner.Child <- container

        new () =
            let outer, label, inner = CreateLegendElements "Group"
            LegendElement (outer, label, inner)

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

        let container   = LayoutElement ()
        let border      = Border ()
        let grid        = Grid ()
        let stackPanel  = CreateStackPanel Orientation.Vertical

        let submitButton= CreateButton "_Submit" "Click to submit form" this.CanSubmit this.Submit
        let resetButton = CreateButton "_Reset" "Click to reset form"   this.CanReset this.Reset

        let errorSymbol = SymbolElement ([|
                                            ("\u26CA", symbolSize       , errorSymbolBackgroundBrush, SymbolTypeFace    )
                                            ("\u26C9", symbolSize       , DefaultBackgroundBrush    , SymbolTypeFace    )
                                            ("\u2757", symbolSize / 2.0 , DefaultBackgroundBrush    , SymbolTypeFace    )
                                        |])
        let okSymbol    = SymbolElement ([|
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
                failures <- value |> List.rev   // TODO: Distinct?
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
                                    Run (" § ")             :> Inline
                                    Run (f.FailureContext   |> LastOrDefault "No context"   )
                                                                :> Inline
                                    Run (" - ")             :> Inline
                                    Run (f.Message)         :> Inline
                                    LineBreak()             :> Inline
                                ])
                        let full =
                            [
                                Run ("You can't submit because")    |?> (fun r -> r.FontSize <- largeSize)
                                                                        :> Inline
                                LineBreak()                         :> Inline
                            ]
                            @ inlines
                        full |>  List.toArray
                    else
                        errorSymbol.Visibility  <- Visibility.Collapsed
                        okSymbol.Visibility     <- Visibility.Visible
                        border.Background       <- okBackgroundBrush
                        border.BorderBrush      <- okBorderBrush
                        [|
                                Run ("Ready to submit")     |?> (fun r -> r.FontSize <- largeSize)
                                                                :> Inline
                                LineBreak()                 :> Inline
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
