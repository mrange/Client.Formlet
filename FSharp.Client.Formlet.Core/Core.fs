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

namespace FSharp.Client.Formlet.Core

open System
open System.Collections
open System.Text.RegularExpressions

type IFormletContext =
    abstract PushTag            : obj   -> unit
    abstract PopTag             : unit  -> unit

type IFormletCache =
    abstract Clear  : unit -> unit

type FormletCache<'T>() =
    let mutable hasValue= false
    let mutable value   = Unchecked.defaultof<'T>

    interface IFormletCache with
        member x.Clear () = x.Clear ()

    member x.Clear () =
        hasValue    <- false
        value       <- Unchecked.defaultof<'T>

    member x.Set (v : 'T) =
        hasValue    <- true
        value       <- v

    member x.HasValue   = hasValue

    member x.Value      = value

type FormletOrientation =
    | Parent
    | TopToBottom
    | LeftToRight

    member this.Union (fo : FormletOrientation) =
        match fo with
        | Parent    -> this
        | _         -> fo

type FormletStretch =
    | Parent
    | NoStretch
    | RightStretches

type FormletDimension =
    | Parent
    | Maximize
    | Minimize
    | Fixed of double

    member this.Union (fd : FormletDimension) =
        match fd with
        | Parent    -> this
        | _         -> fd

type FormletLayout =
    {
        Orientation : FormletOrientation
        Width       : FormletDimension
        Height      : FormletDimension
    }

    static member New orientation width height = { Orientation = orientation; Width = width; Height = height; }

    member this.Union (fl : FormletLayout) =
        FormletLayout.New (this.Orientation.Union fl.Orientation) (this.Width.Union fl.Width) (this.Height.Union fl.Height)

type FormletTree<'Element when 'Element : not struct> =
    | Empty
    | Element   of 'Element
    | Adorner   of 'Element*IList*FormletTree<'Element>[]
    | Layout    of FormletLayout*FormletTree<'Element>
    | Fork      of FormletTree<'Element>*FormletTree<'Element>
    | Modify    of ('Element->unit)*FormletTree<'Element>
    | Tag       of obj*FormletTree<'Element>
    | Group     of string*FormletTree<'Element>
    | Cache     of IFormletCache*FormletTree<'Element>

type FormletFailure =
    {
        FailureContext  : string list
        Message         : string
    }
    static member New (failureContext : string list) (message : string) = { FailureContext = failureContext; Message = message; }

    member x.AddContext (context : string) = FormletFailure.New (context::x.FailureContext) x.Message

type FormletCollect<'T> =
    {
        Value       : 'T
        Failures    : FormletFailure list
    }
    static member New       (value : 'T) (failures : FormletFailure list) = { Value = value; Failures = failures; }
    static member Success   (value : 'T) = FormletCollect.New value []
    static member Failure   (failures : FormletFailure list) = FormletCollect.New Unchecked.defaultof<_> failures
    static member FailWith  (failure : string) = FormletCollect<_>.Failure [FormletFailure.New [] failure]

    member x.HasFailures = not x.Failures.IsEmpty

    member x.AddFailure (formfailure : FormletFailure) = FormletCollect.New x.Value (formfailure::x.Failures)

    member x.AddFailures (formfailures : FormletFailure list) =
        if formfailures.IsEmpty then x
        else FormletCollect.New x.Value (x.Failures @ formfailures)

    member x.AddContext (context : string) =
        if x.Failures.IsEmpty then x
        else
            let failures =
                x.Failures
                |> List.map (fun ff -> ff.AddContext context)
            FormletCollect.New x.Value failures

type FormletChangeNotification = unit -> unit

type Formlet<'Context, 'Element, 'T when 'Context : not struct and 'Context :> IFormletContext and 'Element : not struct> =
    {
        Evaluator : 'Context*FormletChangeNotification*FormletTree<'Element> -> FormletCollect<'T>*FormletTree<'Element>
    }
    static member New evaluator = { Evaluator = evaluator }

    member this.Evaluate    = this.Evaluator

module Formlet =

    let inline New eval : Formlet<'Context, 'Element, 'T> = Formlet<_,_,_>.New eval

    let Return (v : 'T) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) = (FormletCollect.Success v, Empty)
        New eval

    let ReturnFrom (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> = f

    let Bind (f1 : Formlet<'Context, 'Element, 'T1>) (u2 : 'T1 -> Formlet<'Context, 'Element, 'T2>) : Formlet<'Context, 'Element, 'T2> =
        let eval (fc,cl,ft) =
            let ft1, ft2 =
                match ft with
                | Fork (ft1,ft2)    -> ft1, ft2
                | _                 -> Empty, Empty
            let c1, nft1= f1.Evaluate (fc,cl,ft1)
            let f2      = u2 c1.Value
            let c2, nft2= f2.Evaluate (fc,cl,ft2)
            c2.AddFailures c1.Failures, Fork (nft1, nft2)
        New eval

    let Delay (f : unit -> Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =

        let eval (fc,cl,ft) = f().Evaluate (fc,cl,ft)
        New eval

    let MapResult (m : FormletCollect<'T> -> FormletCollect<'U>) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'U> =
        let eval (fc,cl,ft) =
            let c,ift = f.Evaluate (fc,cl,ft)
            (m c), ift
        New eval

    let Map (m : 'T -> 'U) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'U> =
        let m2 collect = { Value = m collect.Value ; Failures = collect.Failures; }
        MapResult m2 f

    let Layout (fl : FormletLayout) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) =
            let ift =
                match ft with
                | Layout (_,ft) -> ft
                | _             -> Empty
            let c,nift = f.Evaluate (fc,cl,ift)
            c,Layout (fl, nift)
        New eval

    let Tag (tag : obj) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc : 'Context,cl,ft) =
            let ift =
                match ft with
                | Tag (_,ft)    -> ft
                | _             -> Empty
            fc.PushTag tag
            let c,nift = f.Evaluate (fc,cl,ift)
            fc.PopTag ()
            c,Tag (tag, nift)
        New eval

    let Group (groupId : string) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) =
            let ift =
                match ft with
                | Group (gid,ft) when groupId = gid -> ft
                | _                                 -> Empty
            let c,nift = f.Evaluate (fc,cl,ift)
            c,Group (groupId, nift)
        New eval

    let Cache (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) =
            let ic,ift =
                match ft with
                | Cache (:? FormletCache<'T> as c,ft)   -> c,ft
                | Cache (c,ft)                          -> c.Clear ()
                                                           FormletCache<'T>(), Empty
                | _                                     -> FormletCache<'T>(), Empty

            if ic.HasValue then
                FormletCollect.Success ic.Value,ft
            else
                let ncl () = ic.Clear (); cl ()
                let c,nift = f.Evaluate (fc,ncl,ift)

                if c.HasFailures then
                    ic.Clear ()
                else
                    ic.Set c.Value

                c,Cache (ic, nift)
        New eval

    let Run (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> = Cache f

    let Validate (validator : 'T -> string option) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) =
            let c,nft = f.Evaluate (fc,cl,ft)
            let v = validator c.Value
            let nc =
                match v with
                | Some failure  -> c.AddFailure (FormletFailure.New [] failure)
                | _             -> c
            nc,nft
        New eval

    let Validate_NonEmpty (f : Formlet<'Context, 'Element, string>) : Formlet<'Context, 'Element, string> =
        Validate (fun s -> if String.IsNullOrWhiteSpace s then Some "Value must not be empty" else None) f

    let Validate_Regex (r : Regex) (msg : string) (f : Formlet<'Context, 'Element, string>) : Formlet<'Context, 'Element, string> =
        Validate (fun s -> if r.IsMatch s then None else Some msg) f

    [<Sealed>]
    type FormletBuilder () =
        member this.Return      x       = Return        x
        member this.Bind        (x, f)  = Bind          x f
        member this.Delay       f       = Delay         f
        member this.ReturnFrom  f       = ReturnFrom    f
        member this.Run         f       = Run           f

    let Do = FormletBuilder ()

[<AutoOpen>]
module FormletAutoOpen =

    let formlet = Formlet.FormletBuilder ()
