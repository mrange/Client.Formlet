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

open FSharp.Client.Formlet.Core

open System
open System.Threading
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Media
open System.Windows.Threading

[<AutoOpen>]
module internal Functions =

    let Enumerator (e : array<'T>) = e.GetEnumerator ()

    let CreateRoutedEvent<'TOwner> name = 
        EventManager.RegisterRoutedEvent (name + "Event", RoutingStrategy.Bubble, typeof<RoutedEventHandler>, typeof<'TOwner>)

    let RaiseRoutedEvent routedEvent (sender : UIElement) = 
        let args = new RoutedEventArgs (routedEvent, sender)
        sender.RaiseEvent args

    let AddRoutedEventHandler routedEvent (receiver : UIElement) (h : obj -> RoutedEventArgs -> unit) = 
        receiver.AddHandler (routedEvent, RoutedEventHandler h)

    let ActionAsDelegate (action : unit -> unit) = 
        let a = Action action 
        let d : Delegate = upcast a
        d

    let Dispatch (dispatcher : Dispatcher) (action : unit -> unit) = 
        let d = ActionAsDelegate action
        ignore <| dispatcher.BeginInvoke (DispatcherPriority.ApplicationIdle, d)

    let HardFail msg            = failwith msg
    let HardFail_InvalidCase () = HardFail "FSharp.Client.Formlet.WPF.ProgrammingError: This case shouldn't be reached"

    let Fail<'T> (msg : string) = FormCollect.New Unchecked.defaultof<'T> [{FailureContext = []; Message = msg;}]
    let Fail_NeverBuiltUp ()    = Fail "FSharp.Client.Formlet.WPF.ProgrammingError: Never built up"

    let EmptySize = new Size ()
    let EmptyRect = new Rect ()

    let TranslateUsingOrientation orientation (fill : bool) (sz : Size) (l : Rect) (r : Size) = 
        match fill,orientation with 
        |   false, TopToBottom  -> Rect (0.0        , l.Bottom  , sz.Width                      , r.Height                      )
        |   false, LeftToRight  -> Rect (l.Right    , 0.0       , r.Width                       , sz.Height                     )
        |   true , TopToBottom  -> Rect (0.0        , l.Bottom  , sz.Width                      , max (sz.Height - l.Bottom) 0.0)
        |   true , LeftToRight  -> Rect (l.Right    , 0.0       , max (sz.Width - l.Right) 0.0  , sz.Height                     )

    let ExceptVertically (l : Size) (r : Size) = 
        Size (max l.Width r.Width, max (l.Height - r.Height) 0.0)

    let ExceptHorizontally (l : Size) (r : Size) = 
        Size (max (l.Width - r.Width) 0.0, max l.Height r.Height)

    let ExceptUsingOrientation (o : FormOrientation) (l : Size) (r : Size) =
        match o with
        |   TopToBottom -> ExceptVertically    l r
        |   LeftToRight -> ExceptHorizontally  l r

    let Intersect (l : Size) (r : Size) = 
        Size (min l.Width r.Width, min l.Height r.Height)

    let Union (l : Size) (r : Size) = 
        Size (max l.Width r.Width, max l.Height r.Height)

    let UnionVertically (l : Size) (r : Size) = 
        Size (max l.Width r.Width, l.Height + r.Height)

    let UnionHorizontally (l : Size) (r : Size) = 
        Size (l.Width + r.Width, max l.Height r.Height)

    let UnionUsingOrientation (o : FormOrientation) (l : Size) (r : Size) =
        match o with
        |   TopToBottom -> UnionVertically    l r
        |   LeftToRight -> UnionHorizontally  l r

    let DefaultBackgroundBrush      = Brushes.White     :> Brush
    let DefaultForegroundBrush      = Brushes.Black     :> Brush
    let DefaultBorderBrush          = Brushes.SkyBlue   :> Brush
    let DefaultErrorBrush           = Brushes.Red       :> Brush

    let DefaultMargin               = Thickness (4.0)

    let DefaultButtonPadding        = Thickness (16.0,2.0,16.0,2.0)

    let DefaultListBoxItemPadding   = Thickness (24.0,0.0,0.0,0.0)

    let DefaultBorderMargin         = Thickness (0.0,8.0,0.0,0.0)
    let DefaultBorderPadding        = Thickness (0.0,16.0,4.0,8.0)
    let DefaultBorderThickness      = Thickness (2.0)

    let DefaultFontFamily           = FontFamily "Segoe UI"
    let SymbolFonFamilyt            = FontFamily "Segoe UI Symbol"
    let DefaultTypeFace             = Typeface   "Segoe UI"
    let SymbolTypeFace              = Typeface   "Segoe UI Symbol"

    let DefaultCulture              = Thread.CurrentThread.CurrentUICulture

    let inline CreateElement<'Element when 'Element :> UIElement> (e : obj) (creator : unit -> 'Element)=
        match e with 
        | :? 'Element as e  -> e
        | _                 -> creator ()

    let FormatText text typeFace fontSize foreGround = 
        let ft = FormattedText (text                                    ,
                                DefaultCulture                          ,
                                FlowDirection.LeftToRight               ,
                                typeFace                                ,
                                fontSize                                ,
                                foreGround
                                )
        ft

    let CreateBrush color = 
        let br = new SolidColorBrush (color)
        br.Freeze ()
        br

    let CreatePen br th = 
        let p = new Pen (br, th)
        p.Freeze ()
        p

    type Command(canExecute : unit -> bool, execute : unit -> unit) = 
        let canExecuteChanged           = new Event<EventHandler, EventArgs> ()

        interface ICommand with

            member this.CanExecute          (ctx : obj)     = canExecute ()
            member this.Execute             (ctx : obj)     = execute ()

            member this.add_CanExecuteChanged(handler)      = CommandManager.RequerySuggested.AddHandler(handler)
            member this.remove_CanExecuteChanged(handler)   = CommandManager.RequerySuggested.RemoveHandler(handler)

    type FormListBoxItem () as this =
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

    type FormListBox () as this = 
        inherit ListBox ()

        do
            this.AlternationCount <- Int32.MaxValue

        override this.GetContainerForItemOverride () =
            new FormListBoxItem () :> DependencyObject

    let CreateListBox () = 
        let listBox             = new FormListBox() :> ListBox
        listBox.Margin          <- DefaultMargin
        listBox.SelectionMode   <- SelectionMode.Extended
        listBox.MinHeight       <- 24.0
        listBox.MaxHeight       <- 240.0
        ScrollViewer.SetVerticalScrollBarVisibility(listBox, ScrollBarVisibility.Visible)
        ScrollViewer.SetHorizontalScrollBarVisibility(listBox, ScrollBarVisibility.Disabled)
        listBox

    let CreateStackPanel orientation =
        let stackPanel = new StackPanel()
        stackPanel.Orientation <- orientation
        stackPanel

    let CreateVerticalStackPanel () =
        CreateStackPanel Orientation.Vertical

    let CreateButton t toolTip canExecute execute = 
        let button      = new Button()
        button.ToolTip  <- toolTip
        button.Content  <- t
        button.Margin   <- DefaultMargin
        button.Padding  <- DefaultButtonPadding
        let handler = ref null
        let onLoaded s e =
            button.Command  <- Command (canExecute, execute)
            button.Loaded.RemoveHandler !handler
        handler := RoutedEventHandler onLoaded
        button.Loaded.AddHandler !handler
        button

    let CreateTextBlock t = 
        let textBlock   = new TextBlock ()
        textBlock.Text  <- t
        textBlock.Margin<- DefaultMargin
        textBlock


    let CreateTextBox t = 
        let textBox     = new TextBox ()
        textBox.Text    <- t
        textBox.Margin  <- DefaultMargin
        textBox

    let CreateLabelTextBox t = 
        let label                   = CreateTextBox t
        label.IsReadOnly            <- true
        label.IsTabStop             <- false
        label.Background            <- Brushes.Transparent
        label.BorderThickness       <- new Thickness 0.0
        label.VerticalAlignment     <- VerticalAlignment.Top
        label.HorizontalAlignment   <- HorizontalAlignment.Left
        label

    let CreateManyElements canExecuteNew executeNew canExecuteDelete executeDelete : ListBox*Panel*Button*Button = 
        let buttons         = CreateStackPanel Orientation.Horizontal
        let newButton       = CreateButton "_New" "Click to create another item" canExecuteNew executeNew
        let deleteButton    = CreateButton "_Delete" "Click to delete the currently selected items" canExecuteDelete executeDelete
        ignore <| buttons.Children.Add newButton
        ignore <| buttons.Children.Add deleteButton
        let listBox         = CreateListBox ()
        listBox, buttons :> Panel, newButton, deleteButton

    let CreateLegendElements t : UIElement*TextBox*Decorator = 
        let label               = CreateLabelTextBox t
        label.Background        <- DefaultBackgroundBrush
        label.RenderTransform   <- new TranslateTransform (8.0, -6.0)
        label.FontSize          <- 16.0
        let border              = new Border ()
        let outer               = new Grid ()
        border.Margin           <- DefaultBorderMargin
        border.Padding          <- DefaultBorderPadding
        border.BorderThickness  <- DefaultBorderThickness
        border.BorderBrush      <- DefaultBorderBrush 
        ignore <| outer.Children.Add(border)
        ignore <| outer.Children.Add(label)
        upcast outer, label, upcast border
