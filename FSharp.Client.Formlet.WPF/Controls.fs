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
open System.Windows.Media

open FSharp.Client.Formlet.Core

open Elements

module internal Controls =

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
(*
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

*)

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

