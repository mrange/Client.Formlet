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

open System.Collections
open System.Windows
open System.Windows.Media

open FSharp.Client.Formlet.Core

module Elements =

    [<AbstractClass>]
    type FormletElement () as this =
        inherit FrameworkElement ()

        do
            this.Margin <- DefaultMargin

        static let submitEvent      = CreateRoutedEvent<FormletElement> "Submit"
        static let resetEvent       = CreateRoutedEvent<FormletElement> "Reset"

        static member SubmitEvent   = submitEvent
        static member ResetEvent    = resetEvent

        static member RaiseSubmit  (sender : UIElement) = RaiseRoutedEvent FormletElement.SubmitEvent   sender
        static member RaiseReset   (sender : UIElement) = RaiseRoutedEvent FormletElement.ResetEvent    sender

        member this.RemoveChild (fe : UIElement) =
            if fe <> null then
                this.RemoveVisualChild (fe)
                this.RemoveLogicalChild (fe)

        member this.AddChild (fe : UIElement) =
            if fe <> null then
                this.AddLogicalChild (fe)
                this.AddVisualChild (fe)

    [<AbstractClass>]
    type DecoratorElement (value : UIElement) as this =
        inherit FormletElement ()

        // TODO: Unnecessary array
        let children = [|value|]

        do
            this.AddChild value

        override this.LogicalChildren = children.GetEnumerator ()

        override this.VisualChildrenCount = children.Length

        override this.GetVisualChild (i : int) = upcast children.[i]

        override this.MeasureOverride (sz : Size) =
            ignore <| base.MeasureOverride sz
            value.Measure (sz)
            value.DesiredSize

        override this.ArrangeOverride (sz : Size) =
            ignore <| base.ArrangeOverride sz
            value.Arrange (Rect (sz))
            sz

