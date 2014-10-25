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

namespace FSharp.Client.Formlet.WindowsForms.TestApp

open System
open System.Windows.Forms

open FSharp.Client.Formlet.Core
open FSharp.Client.Formlet.WindowsForms

module Main =

    let LabeledText lbl text =
        Input.Text text
        |> Formlet.Validate_NonEmpty "Value must not be empty"
//        |> Enhance.WithErrorVisual
//        |> Enhance.WithLabel lbl

    [<EntryPoint>]
    [<STAThread>]
    let main argv =

        let person =
            formlet {
                let! firstName  = LabeledText       "First name"    "Mårten"
                let! lastName   = LabeledText       "Last name"     "Rånge"
                return firstName, lastName
            }
//            |> Enhance.WithLegend "Person"

        use form = new Form ()

        let submit v= printfn "Submit: %A" v
        let formletControl = new FormletControl<_> (submit, person)

        form.Controls.Add (formletControl)
        form.Controls.Add (null);

        ignore <| form.ShowDialog ();

        0
