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
open System.Collections.Generic
open System.Text.RegularExpressions

/// IFormletContext allows adaptations to provide Form-wide Context
///  PushTag/PopTag allows FormLets to add "Tags" to the Context that Formlets in that subtree can peak on
type IFormletContext =
    abstract PushTag            : obj   -> unit
    abstract PopTag             : unit  -> unit

/// IFormletContext is used for caching sub-results in the FormletTree.
///  Doing a full recomputation of the result can be expansive, inserting
///  cache elements can help mitigate the cost
type IFormletCache =
    abstract Clear  : unit -> unit

type FormletCache<'T>() =
    let mutable hasValue= false
    let mutable value   = Unchecked.defaultof<'T>

    interface IFormletCache with
        member this.Clear () = this.Clear ()

    member this.Clear () =
        hasValue    <- false
        value       <- Unchecked.defaultof<'T>

    member this.Set (v : 'T) =
        hasValue    <- true
        value       <- v

    member this.HasValue   = hasValue

    member this.Value      = value

type FormletOrientation =
    | Parent
    | TopToBottom
    | LeftToRight

    member this.Union (fo : FormletOrientation) =
        match fo with
        | Parent    -> this
        | _         -> fo

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

/// The FormletTree is representation of how the Form should be built.
///  Adaptations iterate over the FormletTree and build the actual visual
///  As Forms should preserve even when the FormletTree changes the FormletTree
///  the FormletTree carries the essential input and adorner visuals
///  When an adaptation builds the visual it's important to try reuse the
///  visuals the form tree provides
///  An adaptation may inject additional other visual elements as neeeded.
type FormletTree<'Element when 'Element : not struct> =
    | Empty
    /// A single element, typically this is an Input visual
    | Element   of 'Element
    /// An adorner element that contains a FormletTree, an example can be Label visual
    | Adorner   of 'Element*IList<'Element>*FormletTree<'Element>
    /// An element that represents a visual that replicates a FormLet any number of times
    ///  For instance this could be order rows in an order
    | Many      of 'Element*IReadOnlyList<IList<'Element>*FormletTree<'Element>>
    /// Modifies the layout recursively for this FormletTree
    | Layout    of FormletLayout*FormletTree<'Element>
    /// Joins two FormletTrees, this is typically the result of the Bind operation in the Formlet Monad
    | Fork      of FormletTree<'Element>*FormletTree<'Element>
    /// Modifies an visual element, for instance can be used to add error visual
    ///  if the result contained failures
    | Modify    of ('Element->unit)*FormletTree<'Element>
    /// Names a FormletTree, used to prevent unintentional reuse of the FormletTree state
    | Group     of string*FormletTree<'Element>
    /// Caches the result of the FormletTree
    | Cache     of IFormletCache*FormletTree<'Element>

/// FormletFailure represents an error detected while generating a result
///  Contains a context and a description. The context can be the label text to help the user
///  understand what didn't validate
type FormletFailure =
    {
        FailureContext  : string list
        Message         : string
    }
    static member New (failureContext : string list) (message : string) = { FailureContext = failureContext; Message = message; }

    member this.AddContext (context : string) = FormletFailure.New (context::this.FailureContext) this.Message

/// FormletResult is the result when evaluating a Formlet over a FormTree
///  It contains a potential value and potential failures
type FormletResult<'T> =
    {
        Value       : 'T
        Failures    : FormletFailure list
    }
    static member New       (value : 'T) (failures : FormletFailure list) = { Value = value; Failures = failures; }
    static member Success   (value : 'T) = FormletResult.New value []
    static member Failure   (failures : FormletFailure list) = FormletResult.New Unchecked.defaultof<_> failures
    static member FailWith  (failure : string) = FormletResult<_>.Failure [FormletFailure.New [] failure]

    member this.HasFailures = not this.Failures.IsEmpty

    member this.AddFailure (formfailure : FormletFailure) = FormletResult.New this.Value (formfailure::this.Failures)

    member this.AddFailures (formfailures : FormletFailure list) =
        if formfailures.IsEmpty then this
        else FormletResult.New this.Value (this.Failures @ formfailures)

    member this.AddContext (context : string) =
        if this.Failures.IsEmpty then this
        else
            let failures =
                this.Failures
                |> List.map (fun ff -> ff.AddContext context)
            FormletResult.New this.Value failures

type FormletChangeNotification = unit -> unit

/// Formlet is a generic type of:
///  'Context - A custom context provided by the adaptation, the core formlet library interact very little with the context
///             but is provided in case the adaptation require a global context to flow through
///  'Element - The basic visual element used by the adaptation, for instance in WPF this could be UIElement
///  'T       - The type of the result of the Formlet
type Formlet<'Context, 'Element, 'T when 'Context : not struct and 'Context :> IFormletContext and 'Element : not struct> =
    {
        Evaluator : 'Context*FormletChangeNotification*FormletTree<'Element> -> FormletResult<'T>*FormletTree<'Element>
    }
    static member New evaluator = { Evaluator = evaluator }

    /// Evaluate takes
    ///  context            - The formlet context
    ///  changeNotification - Used by an input visual to signal that user input is received
    ///                       This is typically stored by input elements to signal that something has happened
    ///                       The changeNotification also resets any cached elements in the ancestors
    ///  formletTree        - The existing formletTree, the evaluator is expected to compare the input formlet tree
    ///                       with what it expects. If it matches the input should be modified as necessary, otherwise
    ///                       a new element should be recreated. This is important to retain the state of the Form
    ///                       For example: If the Formlet expects the input to be an Element of a TextBox, the formlet
    ///                       tests for this. If it matches the TextBox is modified if necessary, otherwise a new one
    ///                       is created an initialized
    member this.Evaluate    = this.Evaluator

module FormletMonad =
    let inline New eval : Formlet<'Context, 'Element, 'T> = Formlet<_,_,_>.New eval

    let Return (v : 'T) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) = (FormletResult.Success v, Empty)

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

    let Cache (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) =
            let ic,ift =
                match ft with
                | Cache (:? FormletCache<'T> as c,ft)   -> c,ft
                | Cache (c,ft)                          -> c.Clear ()
                                                           FormletCache<'T>(), Empty
                | _                                     -> FormletCache<'T>(), Empty

            if ic.HasValue then
                FormletResult.Success ic.Value,ft
            else
                let ncl () = ic.Clear (); cl ()
                let c,nift = f.Evaluate (fc,ncl,ift)

                if c.HasFailures then
                    ic.Clear ()
                else
                    ic.Set c.Value

                c,Cache (ic, nift)

        New eval

    // TODO: I want to Cache here but the trivial attempt don't work with slightly more
    // complex formlets where a formlet returns a formlet that is later resolved
    let Run (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> = f

    [<Sealed>]
    type FormletBuilder () =
        member this.Return      x       = Return        x
        member this.Bind        (x, f)  = Bind          x f
        member this.Delay       f       = Delay         f
        member this.ReturnFrom  f       = ReturnFrom    f
        member this.Run         f       = Run           f

module Formlet =

    /// MapResult maps the result of Formlet from one type into another type
    let MapResult (m : FormletResult<'T> -> FormletResult<'U>) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'U> =
        let eval (fc,cl,ft) =
            let c,ift = f.Evaluate (fc,cl,ft)
            (m c), ift
        FormletMonad.New eval

    /// Map maps the value of a Formlet from one type into another type
    let Map (m : 'T -> 'U) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'U> =
        let im (result : FormletResult<_>) =
            if result.HasFailures then
                FormletResult<_>.Failure result.Failures
            else
                FormletResult.Success (m result.Value)
        MapResult im f

    /// Layout modifies the layout of the FormTree
    let Layout (fl : FormletLayout) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) =
            let ift =
                match ft with
                | Layout (_,ft) -> ft
                | _             -> Empty
            let c,nift = f.Evaluate (fc,cl,ift)
            c,Layout (fl, nift)

        FormletMonad.New eval

    /// Tag adds a custom tag to the FormletContext
    let Tag (tag : obj) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc : 'Context,cl,ft) =
            fc.PushTag tag
            let c,nift = f.Evaluate (fc,cl,ft)
            fc.PopTag ()
            c,nift

        FormletMonad.New eval

    /// Names a FormletTree, used to prevent unintentional reuse of the FormletTree state
    let Group (groupId : string) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) =
            let ift =
                match ft with
                | Group (gid,ft) when groupId = gid -> ft
                | _                                 -> Empty
            let c,nift = f.Evaluate (fc,cl,ift)
            c,Group (groupId, nift)

        FormletMonad.New eval

    /// Validates the result against a validator function. If the validator returns Some text then
    /// this is displayed to the user
    let Validate (validator : 'T -> string option) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> =
        let eval (fc,cl,ft) =
            let c,nft = f.Evaluate (fc,cl,ft)
            let v = validator c.Value
            let nc =
                match v with
                | Some failure  -> c.AddFailure (FormletFailure.New [] failure)
                | _             -> c
            nc,nft

        FormletMonad.New eval

    /// Validates that the result is not an empty string
    let Validate_NonEmpty (msg : string) (f : Formlet<'Context, 'Element, string>) : Formlet<'Context, 'Element, string> =
        Validate (fun s -> if String.IsNullOrWhiteSpace s then Some msg else None) f

    /// Validates that the result matches a Regex
    let Validate_Regex (r : Regex) (msg : string) (f : Formlet<'Context, 'Element, string>) : Formlet<'Context, 'Element, string> =
        Validate (fun s -> if r.IsMatch s then None else Some msg) f

    /// Validate_Optionmaps 'T option to 'T
    let Validate_Option (defaultValue : 'T) (msg : string) (f : Formlet<'Context, 'Element, 'T option>) : Formlet<'Context, 'Element, 'T> =
        let m result =
            match result.Value, result.Failures with
            | Some v, []-> FormletResult.Success v
            | _     , fs-> FormletResult.New defaultValue ((FormletFailure.New [] msg)::fs)
        MapResult m f

[<AutoOpen>]
module FormletAutoOpen =

    let formlet = FormletMonad.FormletBuilder ()
