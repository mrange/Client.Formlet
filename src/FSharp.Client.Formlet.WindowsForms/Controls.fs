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

namespace FSharp.Client.Formlet.WindowsForms

open System
open System.Collections
open System.Collections.Generic
open System.Drawing
open System.Windows.Forms

open FSharp.Client.Formlet.Core

module Controls =

    let EmptyChangeNotification : FormletChangeNotification = fun () -> ()

    type LayoutControl () as this =
        inherit Control ()

        let mutable vertical            = true
        let mutable expandLast          = true

        let invalidate () =
            this.Invalidate ()

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

        override this.InitLayout () =
            base.InitLayout ()

        override this.GetPreferredSize (proposedSize : Size) =
            base.GetPreferredSize proposedSize

        override this.OnLayout le =
            base.OnLayout le

    type InputTextControl(initialText : string) as this =
        inherit TextBox()

        let mutable text                = initialText

        do
            this.Text <- initialText

        member val ChangeNotifier = EmptyChangeNotification with get, set


        override this.OnLostFocus (e) =
            base.OnLostFocus(e)

            if text <> this.Text then
                text <- this.Text

                this.ChangeNotifier ()
