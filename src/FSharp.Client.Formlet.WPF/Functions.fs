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
open System.Threading
open System.Windows
open System.Windows.Controls
open System.Windows.Documents
open System.Windows.Input
open System.Windows.Media
open System.Windows.Threading

open FSharp.Client.Formlet.Core

[<AutoOpen>]
module internal Functions =

    let inline ( |?> ) (x : 'a) (y : 'a -> unit) =
        y x
        x

    let EmptyChangeNotification : FormletChangeNotification = fun () -> ()

    let CreateRoutedEvent<'TOwner> name =
        EventManager.RegisterRoutedEvent (name + "Event", RoutingStrategy.Bubble, typeof<RoutedEventHandler>, typeof<'TOwner>)

    let RaiseRoutedEvent routedEvent (sender : UIElement) =
        let args = RoutedEventArgs (routedEvent, sender)
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

    let Fail<'T> (msg : string) = FormletResult<_>.FailWith msg
    let Fail_NeverBuiltUp ()    = Fail "FSharp.Client.Formlet.WPF.ProgrammingError: Never built up"

    let rec LastOrDefault defaultTo ls =
        match ls with
        |   []          -> defaultTo
        |   [v]         -> v
        |   _::vs       -> LastOrDefault defaultTo vs

    let EmptySize = Size ()
    let EmptyRect = Rect ()

    let Arrange (vertical : bool, expand : bool, sz : Size, l : Rect, r : Size) : Rect =
        match expand,vertical with
        |   false, true     -> Rect (0.0        , l.Bottom  , sz.Width                      , r.Height                      )
        |   false, false    -> Rect (l.Right    , 0.0       , r.Width                       , sz.Height                     )
        |   true , true     -> Rect (0.0        , l.Bottom  , sz.Width                      , max (sz.Height - l.Bottom) 0.0)
        |   true , false    -> Rect (l.Right    , 0.0       , max (sz.Width - l.Right) 0.0  , sz.Height                     )

    let inline ExceptVertically (l : Size, r : Size) : Size =
        Size (max l.Width r.Width, max (l.Height - r.Height) 0.0)

    let inline ExceptHorizontally (l : Size, r : Size) : Size =
        Size (max (l.Width - r.Width) 0.0, max l.Height r.Height)

    let ExceptSize (vertical : bool, l : Size, r : Size) : Size =
        if vertical then    ExceptVertically (l,r)
        else                ExceptHorizontally (l,r)

    let Intersect (l : Size) (r : Size) =
        Size (min l.Width r.Width, min l.Height r.Height)

    let Union (l : Size) (r : Size) =
        Size (max l.Width r.Width, max l.Height r.Height)

    let inline UnionVertically (l : Size, r : Size) =
        Size (max l.Width r.Width, l.Height + r.Height)

    let inline UnionHorizontally (l : Size, r : Size) =
        Size (l.Width + r.Width, max l.Height r.Height)

    let UnionSize (vertical : bool, l : Size, r : Size) : Size =
        if vertical then    UnionVertically (l,r)
        else                UnionHorizontally (l,r)

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
    let SymbolFontFamily            = FontFamily "Segoe UI Symbol"
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

    let ToNibble ch =
        match ch with
        | c when Char.IsDigit (c)   -> byte  c - byte '0'
        | 'a'   -> byte 0xA
        | 'b'   -> byte 0xB
        | 'c'   -> byte 0xC
        | 'd'   -> byte 0xD
        | 'e'   -> byte 0xE
        | 'f'   -> byte 0xF
        | 'A'   -> byte 0xA
        | 'B'   -> byte 0xB
        | 'C'   -> byte 0xC
        | 'D'   -> byte 0xD
        | 'E'   -> byte 0xE
        | 'F'   -> byte 0xF
        |   _   -> byte 0

    let ExpandNibble (nibble : byte) =
        (nibble <<< 4) ||| (nibble &&& byte 0xF)

    let ToByteFromChar = ToNibble >> ExpandNibble

    let ToByteFromChars (left : char) (right : char) =
        let left' = ToNibble left
        let right' = ToNibble right
        (left' <<< 4) ||| (right' &&& byte 0xF)

    let CreateColor (color : string) =
        let (|ARGB|RGB|LARGB|LRGB|NOCOLOR|) (color : string) =
            match color with
            | ""                    -> NOCOLOR
            | c when c.[0] <> '#'   -> NOCOLOR
            | c ->
                match c.Length with
                | 4 -> RGB
                | 5 -> ARGB
                | 7 -> LRGB
                | 9 -> LARGB
                | _ -> NOCOLOR

        match color with
        | RGB   -> Color.FromRgb    (ToByteFromChar color.[1]           , ToByteFromChar color.[2]              , ToByteFromChar color.[3]              )
        | ARGB  -> Color.FromArgb   (ToByteFromChar color.[1]           , ToByteFromChar color.[2]              , ToByteFromChar color.[3]              , ToByteFromChar color.[4]              )
        | LRGB  -> Color.FromRgb    (ToByteFromChars color.[1] color.[2], ToByteFromChars color.[3] color.[4]   , ToByteFromChars color.[5] color.[6]   )
        | LARGB -> Color.FromArgb   (ToByteFromChars color.[1] color.[2], ToByteFromChars color.[3] color.[4]   , ToByteFromChars color.[5] color.[6]   , ToByteFromChars color.[7] color.[8]   )
        | _ -> Colors.Red

    let CreateBrush color =
        let br = SolidColorBrush (color)
        br.Freeze ()
        br

    let CreateSimpleGradient fromColor toColor =
        let br = LinearGradientBrush (fromColor, toColor, 90.0)
        br.Freeze ()
        br

    let AddGridColumn w (grid : Grid) =
        let gridColumn = ColumnDefinition ()
        gridColumn.Width <- w
        grid.ColumnDefinitions.Add gridColumn
        grid

    let AddGridColumn_Auto g =
        AddGridColumn GridLength.Auto g

    let AddGridColumn_Star w g =
        AddGridColumn (GridLength (w, GridUnitType.Star)) g

    let AddGridColumn_Pixel w g =
        AddGridColumn (GridLength (w, GridUnitType.Pixel)) g

    let AddGridChild ch c r (grid : Grid) =
        ignore <| Grid.SetColumn    (ch, c)
        ignore <| Grid.SetRow       (ch, r)
        ignore <| grid.Children.Add ch
        grid

    let CreatePen br th =
        let p = Pen (br, th)
        p.Freeze ()
        p

    [<AllowNullLiteral>]
    type ErrorVisualAdorner(adornedElement) as this =
        inherit Adorner(adornedElement)

        static let pen = CreatePen DefaultErrorBrush 2.0

        do
            this.IsHitTestVisible <- true

        override this.OnRender (drawingContext) =
            let rect = Rect (this.AdornedElement.RenderSize)
            drawingContext.DrawRectangle (null, pen, rect)

    let GetErrorAdorner (layer : AdornerLayer) (e : UIElement) : Adorner option =
        if layer <> null then
            let adorners = layer.GetAdorners e
            if adorners <> null then
                let findAdorner (adorner : Adorner) =
                    match adorner with
                    | :? ErrorVisualAdorner -> true
                    | _                     -> false
                adorners |> Array.tryFind findAdorner
            else
                None
        else
            None

    let FindAdorner (layer : AdornerLayer) (e : UIElement) : #Adorner =
        if layer <> null then
            let adorners = layer.GetAdorners e
            if adorners <> null then
                let mutable iter    = 0
                let mutable result  = null
                while (Object.ReferenceEquals(result, null)) && iter < adorners.Length do
                    match adorners.[iter] with
                    | :? #Adorner as a -> result <- a
                    | _ -> ()
                    iter <- iter + 1

                result
            else
                null
        else
            null

    let UpdateLoadedAdorner (updater : FrameworkElement*AdornerLayer*#Adorner->unit, fe : FrameworkElement) =
        let layer   = AdornerLayer.GetAdornerLayer fe
        if layer <> null then
            let adorner = FindAdorner layer fe
            updater (fe, layer, adorner)

    type DelayedAdornerUpdater<'Adorner when 'Adorner :> Adorner and 'Adorner : null>(updater : FrameworkElement*AdornerLayer*'Adorner->unit, fe : FrameworkElement) as this =
        let onLoaded sender args =
            fe.Loaded.RemoveHandler this.OnLoaded
            UpdateLoadedAdorner (updater, fe)

        member this.OnLoaded = RoutedEventHandler onLoaded

    let UpdateAdorner (updater : FrameworkElement*AdornerLayer*#Adorner->unit) (e : UIElement) : unit =
        match e with
        | :? FrameworkElement as fe ->
            if fe.IsLoaded then UpdateLoadedAdorner (updater, fe)
            else
                let updater = DelayedAdornerUpdater (updater, fe)
                fe.Loaded.AddHandler updater.OnLoaded
        | _ -> ()


    let AppendErrorAdornerUpdater (fe : FrameworkElement, layer : AdornerLayer, adorner : ErrorVisualAdorner) =
        match adorner with
        | null  -> layer.Add (ErrorVisualAdorner (fe))
        | _     -> ()

    let AppendErrorAdorner (e : UIElement) : unit =
        UpdateAdorner AppendErrorAdornerUpdater e

    let RemoveErrorAdornerUpdater (fe : FrameworkElement, layer : AdornerLayer, adorner : ErrorVisualAdorner) =
        match adorner with
        | null  -> ()
        | _     -> layer.Remove adorner

    let RemoveErrorAdorner (e : UIElement) : unit =
        UpdateAdorner RemoveErrorAdornerUpdater e

    type Command(canExecute : unit -> bool, execute : unit -> unit) =
        let canExecuteChanged   = Event<EventHandler, EventArgs> ()

        interface ICommand with

            member this.CanExecute          (ctx : obj)     = canExecute ()
            member this.Execute             (ctx : obj)     = execute ()

            member this.add_CanExecuteChanged(handler)      = CommandManager.RequerySuggested.AddHandler(handler)
            member this.remove_CanExecuteChanged(handler)   = CommandManager.RequerySuggested.RemoveHandler(handler)

    let CreateStackPanel orientation =
        let stackPanel = StackPanel()
        stackPanel.Orientation <- orientation
        stackPanel

    let CreateButton t toolTip canExecute execute =
        let button      = Button()
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
        let textBlock   = TextBlock ()
        textBlock.Text  <- t
        textBlock.Margin<- DefaultMargin
        textBlock


    let CreateTextBox t =
        let textBox     = TextBox ()
        textBox.Text    <- t
        textBox.Margin  <- DefaultMargin
        textBox

    let CreateLabelTextBox t =
        let label                   = CreateTextBox t
        label.IsReadOnly            <- true
        label.IsTabStop             <- false
        label.Background            <- Brushes.Transparent
        label.BorderThickness       <- Thickness 0.0
        label.VerticalAlignment     <- VerticalAlignment.Top
        label.HorizontalAlignment   <- HorizontalAlignment.Left
        label

    type SingleDispatchQueue<'DispatchEnum when 'DispatchEnum : enum<int32> and 'DispatchEnum : equality> (dispatcher : Dispatcher) =
        let mutable isDispatching   = false
        let queue                   = Queue<'DispatchEnum*(unit->unit)> ()

        member this.Dispatch (dispatchEnum : 'DispatchEnum, action : unit->unit) =
            dispatcher.VerifyAccess ()
            let isAlreadyDispatching = queue |> Seq.exists (fun (de,_) -> de = dispatchEnum)
            if not isAlreadyDispatching then
                queue.Enqueue(dispatchEnum, action)
                this.StartDispatchIfNecessary ()

        member private this.StartDispatchIfNecessary () =
            if not isDispatching && queue.Count > 0 then
                isDispatching <- true
                let _,action = queue.Peek ()
                Dispatch dispatcher <| fun () ->
                    try
                        action ()
                    finally
                        ignore <| queue.Dequeue ()
                        isDispatching <- false
                        this.StartDispatchIfNecessary ()




