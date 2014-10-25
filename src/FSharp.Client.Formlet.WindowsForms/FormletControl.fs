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


open System.Windows.Forms

open FSharp.Client.Formlet.Core

type FormletContext () =
    interface IFormletContext with
        member this.PushTag tag = ()
        member this.PopTag ()   = ()

type FormletControl<'TValue> (submit : 'TValue -> unit, formlet : Formlet<FormletContext, Control, 'TValue>) as this =
    inherit Control()
    let x = 3


