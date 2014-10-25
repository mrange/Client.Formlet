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
open System.Windows.Forms

open FSharp.Client.Formlet.Core

open Controls

module Input =

    let Text initialText : Formlet<FormletContext, Control, string> =
        let eval (fc,cl,ft : FormletTree<Control>) =
            let e =
                match ft with
                | Element (:? InputTextControl as e)-> e
                | _                                 ->
                    new InputTextControl(initialText)
            e.ChangeNotifier <- cl
            (FormletResult.Success e.Text), Element (e :> Control)

        FormletMonad.New eval

    let Integer v =
        let map (collect : FormletResult<string>) : FormletResult<int> =
            if collect.HasFailures then
                FormletResult.New 0 collect.Failures
            else
                let mutable i = 0
                if Int32.TryParse (collect.Value, &i) then
                    FormletResult.Success i
                else
                    FormletResult<_>.FailWith "Input is not an integer"    // TODO: Localization
        Text (v.ToString())
        |> Formlet.MapResult map
        |> FormletMonad.Cache

