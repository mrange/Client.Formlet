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

open System.Collections

type FormOrientation = 
    | Parent
    | TopToBottom
    | LeftToRight

    member this.Union (fo : FormOrientation) = 
        match fo with
        | Parent    -> this
        | _         -> fo

type FormStretch = 
    | Parent
    | NoStretch
    | RightStretches

type FormDimension =
    | Parent
    | Maximize
    | Minimize
    | Size of double

    member this.Union (fd : FormDimension) = 
        match fd with
        | Parent    -> this
        | _         -> fd

type FormLayout =
    {
        Orientation : FormOrientation
        Width       : FormDimension
        Height      : FormDimension
    }

    static member New orientation width height = { Orientation = orientation; Width = width; Height = height; }

    member this.Union (fl : FormLayout) = 
        FormLayout.New (this.Orientation.Union fl.Orientation) (this.Width.Union fl.Width) (this.Height.Union fl.Height)

(*
type FormContext =
    {
        Layout  : FormLayout
    }
    static member New layout = { Layout = layout; }
*)
type FormTree<'Element when 'Element : not struct> = 
    | Empty
    | Singleton of 'Element
    | Container of 'Element*IList*FormTree<'Element> list
    | Layout    of FormLayout*FormTree<'Element>
    | Label     of string*FormTree<'Element>
    | Fork      of FormTree<'Element>*FormTree<'Element>

type VisualTree<'Element when 'Element : not struct> = 
    | NoVisual
    | Visual    of 'Element*VisualTree<'Element> list

module FormTree = 
    let x = 3

(*
    let rec Simplify (layout : FormLayout) (ft : FormTree<'Element>) = 
        match ft with
        | Empty                 -> ft
        | Singleton _           -> ft
        | Container (e, ls, fts)-> Container (e, ls, fts |> List.map (Simplify layout))
        | Layout (l, ift) -> 
            let nl = layout.Union l
            if nl = layout then Simplify nl ift
            else Layout (l, Simplify nl ift)
        | Label (l, ft)         -> Label (l, Simplify layout ft)
        | Fork (l,r)            -> Fork (Simplify layout l, Simplify layout r)
    let (|Group|) (ft : FormTree<'Element>) : FormTree<'Element> list = 
        let rec group (ft : FormTree<'Element>) =
            match ft with
            | Empty                 -> []
            | Singleton _           -> [ft]
            | Container _           -> [ft]
            | Layout _              -> [ft]
            | Label _               -> [ft]
            | Fork (l,r)            -> (group l)@(group r)
        group ft
*)
    
type FormFailure =
    {
        FailureContext  : string list
        Message         : string
    }
    static member New (failureContext : string list) (message : string) = { FailureContext = failureContext; Message = message; }

type FormCollect<'T> =
    {
        Value       : 'T
        Failures    : FormFailure list
    }
    static member New (value : 'T) (failures : FormFailure list) = { Value = value; Failures = failures; }

type Formlet<'Context, 'Element, 'T when 'Element : not struct> = 
    {
        Evaluator : 'Context*FormTree<'Element> -> FormCollect<'T>*FormTree<'Element>
    }
    static member New evaluator = { Evaluator = evaluator }

    member this.Evaluate    = this.Evaluator

module Formlet = 

    let inline New eval : Formlet<'Context, 'Element, 'T> = Formlet<_,_,_>.New eval

    let inline Success (v : 'T) : FormCollect<'T> = FormCollect<_>.New v []

    let Return (v : 'T) : Formlet<'Context, 'Element, 'T> = 
        let eval (fc,ft)    = (Success v, Empty)
        New eval

    let ReturnFrom (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> = f

    let Bind (f1 : Formlet<'Context, 'Element, 'T1>) (u2 : 'T1 -> Formlet<'Context, 'Element, 'T2>) : Formlet<'Context, 'Element, 'T2> = 
        let eval (fc,ft) = 
            let ft1, ft2 = 
                match ft with
                | Fork (ft1,ft2)    -> ft1, ft2
                | _                 -> Empty, Empty
            let c1, nft1= f1.Evaluate (fc,ft1)
            let f2      = u2 c1.Value
            let c2, nft2= f2.Evaluate (fc,ft2)
            c2, Fork (nft1, nft2)
        New eval

    let Delay (f : unit -> Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> = 
        
        let eval (fc,ft)    = f().Evaluate (fc,ft)
        New eval

    let MapResult (m : FormCollect<'T> -> FormCollect<'U>) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'U> = 
        let eval (fc,ft) = 
            let c,ift = f.Evaluate (fc,ft)
            (m c), ift
        New eval

    let Map (m : 'T -> 'U) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'U> = 
        let m2 collect = { Value = m collect.Value ; Failures = collect.Failures; }
        MapResult m2 f

    let Layout (fl : FormLayout) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> = 
        let eval (fc,ft) = 
            let ift = 
                match ft with
                | Layout (_,ft) -> ft
                | _             -> Empty
            let c,nift = f.Evaluate (fc,ift)
            c,Layout (fl, nift)
        New eval

    let Label (l : string) (f : Formlet<'Context, 'Element, 'T>) : Formlet<'Context, 'Element, 'T> = 
        let eval (fc,ft) = 
            let ift = 
                match ft with
                | Label (_,ft)  -> ft
                | _             -> Empty
            let c,nift = f.Evaluate (fc,ift)
            c,Label (l, nift)
        New eval

    [<Sealed>]
    type FormletBuilder () =
        member this.Return      x       = Return        x
        member this.Bind        (x, f)  = Bind          x f
        member this.Delay       f       = Delay         f
        member this.ReturnFrom  f       = ReturnFrom    f

    let Do = FormletBuilder ()

[<AutoOpen>]
module FormletAutoOpen =

    let formlet = Formlet.FormletBuilder ()
