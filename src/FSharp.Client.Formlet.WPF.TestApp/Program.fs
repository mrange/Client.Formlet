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

namespace FSharp.Client.Formlet.WPF.TestApp

open System
open System.Windows

open FSharp.Client.Formlet.Core
open FSharp.Client.Formlet.WPF

module Main =

    let LabeledText lbl text =
        Input.Text text
        |> Formlet.Validate_NonEmpty "Value must not be empty"
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    let LabeledInteger lbl n =
        Input.Integer n
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    let LabeledBirthDate lbl dt =
        Input.DateTime dt
        |> Formlet.Validate (fun dt -> if DateTime.Now < dt then Some "Select a date before today" else None)
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    let LabeledOption lbl defaultValue options =
        Input.Option -1 options
        |> Formlet.Validate_Option defaultValue "Select an option"
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

    let YesNo lbl initial =
        Input.TriState lbl initial
        |> Formlet.Validate_Option false "Checkbox must be ticked or unticked"
        |> Enhance.WithErrorVisual

    [<EntryPoint>]
    [<STAThread>]
    let main argv =

        let person =
            formlet {
                let! firstName  = LabeledText       "First name"    "John"
                let! lastName   = LabeledText       "Last name"     "Doe"
                let! birthDate  = LabeledBirthDate  "Birth date"    None
                return firstName, lastName, birthDate
            }
            |> Enhance.WithLegend "Person"

        let address =
            formlet {
                let! street = LabeledText       "Street"    ""
                let! zip    = LabeledText       "Zip"       ""
                return street, zip
            }
            |> Enhance.WithLegend "Address info"

        let configurations =
            [
                "Sweden"    ,   ["Org No"                       ]
                "Norway"    ,   ["Org No"   ; "MVA No"          ]
                "Narnia"    ,   ["Aslan Pts" ; "Ice queen Pts"  ]
            ]

        let configuration (options : string list) =
            let formlets =
                options
                |> List.map (fun option -> LabeledText option "" |> Formlet.Map (fun s -> option,s))
                |> List.toArray
            formlet {
                let! result = Formlet.ForEach formlets
                return result
            }

        let options =
            configurations
            |> List.map (fun (country, options) -> country, configuration options)
            |> List.toArray

        let empty =
            formlet {
                return [||]
            }

        let companyInfo =
            let options = LabeledOption "Country" empty options
            formlet {
                let! country    = options
                let! name       = LabeledText   "Name"      ""
                let! values     = country
                return name, values
            }
            |> Enhance.WithLegend "Company info"

        let addresses =
            let address =
                formlet {
                    let! street = LabeledText       "Street"    ""
                    let! zip    = LabeledText       "Zip"       ""
                    return street, zip
                }
            address
            |> Enhance.Many 1
            |> Formlet.Validate (fun vs -> if vs.Length > 0 then None else Some "At least one address is required")
            |> Enhance.WithLegend "Addresses"

        let full =
            formlet {
                let! firstName, lastName, birthDate = person
                let! addresses                      = addresses
                let! name, values                   = companyInfo
                return firstName, lastName, addresses, birthDate, name, values
            }

        let sample1 =
            formlet {
                let! name = Input.Text "" |> Enhance.WithLabel "Your name"
                return name
            }

        let sample2 =
            formlet {
                let! name = LabeledText "Your name" ""

                return name
            }

        let sample3 =
            formlet {
                let! person     = person
                let! address    = address
                return person, address
            }

        let sample4 =
            let address lbl =
                formlet {
                    let! street = LabeledText       "Street"    ""
                    let! zip    = LabeledText       "Zip"       ""
                    return street, zip
                }
                |> Enhance.WithLegend lbl

            formlet {
                let! deliveryAddress    = address "Delivery Address"
                let! hasInvoiceAddress  = YesNo "Separate invoice/delivery address?" (Some false)
                let! invoiceAddress     =
                    if hasInvoiceAddress then
                        address "Invoice Address" |> Formlet.Map Some
                    else
                        Formlet.Return None
                return deliveryAddress, invoiceAddress
            }

        let complete = full |> Enhance.WithErrorSummary

        let window  = Window ()
        let submit v= printfn "Submit: %A" v
        window.Content <- FormletControl(submit, complete)

        ignore <| window.ShowDialog ()

        0
