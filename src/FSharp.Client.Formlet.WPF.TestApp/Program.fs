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
    let LabeledOptText lbl text =
        Input.Text text
        |> Enhance.WithErrorVisual
        |> Enhance.WithLabel lbl

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

    let LabeledExpireDate lbl dt =
        Input.DateTime dt
        |> Formlet.Validate (fun dt -> if DateTime.Now > dt then Some "Select a date after today" else None)
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

    module RegistrationFlow =
        type AddressInfo =
            {
                FirstName       : string
                LastName        : string
                CareOf          : string
                AddressLine1    : string
                AddressLine2    : string
                AddressLine3    : string
                Zip             : string
                City            : string
                Country         : string
            }

        type GenderInfo =
            | Unspecified
            | Male
            | Female

        type PersonInfo =
            {
                Id              : string
                FirstName       : string
                LastName        : string
                Gender          : GenderInfo
                BirthDate       : DateTime
                CellPhone       : string
                Email           : string
            }

        type CompanyInfo =
            {
                Id              : string
                VatId           : string
                Name            : string
                Contact         : PersonInfo
            }

        type EntityInfo =
            | Person    of PersonInfo
            | Company   of CompanyInfo

        type PaymentInfo =
            | CreditCard    of string*string*DateTime*string
            | Invoice       of AddressInfo option


        type RegistrationInfo =
            {
                Entity          : EntityInfo
                DeliveryAddress : AddressInfo
                Payment         : PaymentInfo
            }

        let showPage (f : Formlet<'T>) =
            f
            |> Enhance.WithErrorSummary true
            |> EnhancePage.WithNavigationButtons
            |> Flowlet.Show

        let personForm legend firstName lastName =
            formlet {
                let! id         = LabeledText       "Social No"     ""
                let! firstName  = LabeledText       "First name"    firstName
                let! lastName   = LabeledText       "Last name"     lastName
                let! gender     = LabeledOption     "Gender"        Unspecified [|"Unspecified",Unspecified; "Female",Female; "Male",Male|]
                let! birthDate  = LabeledBirthDate  "Birth date"    None
                let! cellPhone  = LabeledText       "Cell"          ""
                let! email      = LabeledText       "Email"         ""
                return
                    {
                        Id          = id
                        FirstName   = firstName
                        LastName    = lastName
                        Gender      = gender
                        BirthDate   = birthDate
                        CellPhone   = cellPhone
                        Email       = email
                    }
            }
            |> Enhance.WithLegend legend

        let companyForm legend =
            formlet {
                let! id            = LabeledText       "Company No"     ""
                let! vatId         = LabeledText       "Vat No"         ""
                let! name          = LabeledText       "Company name"   ""
                return id, vatId, name
            }
            |> Enhance.WithLegend legend

        let addressForm legend firstName lastName =
            formlet {
                let! firstName        = LabeledText     "First name"      firstName
                let! lastName         = LabeledText     "Last name"       lastName
                let! careOf           = LabeledOptText  "C/O"             ""
                let! addressLine1     = LabeledText     "Address"         ""
                let! addressLine2     = LabeledOptText  "Address"         ""
                let! addressLine3     = LabeledOptText  "Address"         ""
                let! zip              = LabeledText     "Zip"             ""
                let! city             = LabeledText     "City"            ""
                let! country          = LabeledText     "Country"         ""
                return
                    {
                        FirstName       = firstName
                        LastName        = lastName
                        CareOf          = careOf
                        AddressLine1    = addressLine1
                        AddressLine2    = addressLine2
                        AddressLine3    = addressLine3
                        Zip             = zip
                        City            = city
                        Country         = country
                    }
            }
            |> Enhance.WithLegend legend

        let creditCardForm legend firstName lastName =
            formlet {
                let! name           = LabeledText       "Name"          <| firstName + " " + lastName
                let! no             = LabeledText       "No"            ""
                let! expireDate     = LabeledExpireDate "Expire date"   None
                let! cvc            = LabeledText       "CVC"           ""
                return CreditCard (name, no, expireDate, cvc)
            }

        let invoiceForm legend firstName lastName =
            formlet {
                let! useDeliveryAddress = YesNo "Invoice address is same as delivery address" <| Some true
                if not useDeliveryAddress then
                    let! address = addressForm "Invoice address" firstName lastName
                    return Some address
                else
                    return None
            }

        let personFlow firstName lastName =
            flowlet {
               let! person = showPage <| personForm "Personal information" firstName lastName
               return Person person
            }

        let companyFlow firstName lastName =
            flowlet {
                let! id, vatId, name= showPage <| companyForm  "Company information"
                let! contact        = showPage <| personForm   "Contact information" firstName lastName
                let company =
                    {
                        Id      = id
                        VatId   = vatId
                        Name    = name
                        Contact = contact
                    }
               return Company company
            }

        let creditCardFlow firstName lastName =
            flowlet {
               let! creditCard = showPage <| creditCardForm "Credit card information" firstName lastName
               return creditCard
            }

        let invoiceFlow firstName lastName =
            flowlet {
               let! invoice = showPage <| invoiceForm "Invoice information" firstName lastName
               return Invoice invoice
            }

        let selectForm =
            formlet {
                let! entityFlow     = LabeledOption "Entity"    personFlow      [|"Person",personFlow; "Company",companyFlow|]
                let! paymentFlow    = LabeledOption "Payment"   creditCardFlow  [|"Credit card",creditCardFlow; "Invoice",invoiceFlow|]
                return entityFlow, paymentFlow
            }

        let registrationFlow =
            flowlet {
                let! entityFlow, paymentFlow= showPage selectForm
                let! deliveryAddress        = showPage <| addressForm "Delivery address" "" ""
                let! entity                 = entityFlow deliveryAddress.FirstName deliveryAddress.LastName
                let! payment                = paymentFlow deliveryAddress.FirstName deliveryAddress.LastName
                return
                    {
                        Entity              = entity
                        DeliveryAddress     = deliveryAddress
                        Payment             = payment
                    }
            }


(*

        let flow =
            flowlet {
                let! firstName, lastName, birthDate = showPage person
                let! addresses                      = showPage addresses
                let! name, values                   = showPage companyInfo
                return firstName, lastName, addresses, birthDate, name, values
            }
*)

        let lastPage v =
            formlet {
                do! Static.Text (sprintf "We are done\n%A" v) 6
                return v
            }
            |> Enhance.WithErrorSummary true
            |> EnhancePage.WithSubmitButtons

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

            let invoiceAddress hasInvoiceAddress =
                formlet {
                    if hasInvoiceAddress then
                        let! add = address "Invoice Address"
                        return Some add
                    else
                        return None
                }

            formlet {
                let! deliveryAddress    = address "Delivery Address"
                let! hasInvoiceAddress  = YesNo "Separate invoice/delivery address?" (Some false)
                let! invoiceAddress     = invoiceAddress hasInvoiceAddress
                return deliveryAddress, invoiceAddress
            }

        let complete = full |> Enhance.WithErrorSummary true |> Enhance.WithSubmitButtons

        let window      = Window ()
        let submit v    = printfn "Submit: %A" v
        let cancel ()   = printfn "Cancelled"
//        window.Content <- FormletControl.Create submit cancel complete
        window.Content <- FlowletControl.Create submit cancel RegistrationFlow.registrationFlow RegistrationFlow.lastPage

        ignore <| window.ShowDialog ()

        0
