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

open System
open System.Windows

open FSharp.Client.Formlet.Core

open InternalElements

module Input =

    let Text initialText : Formlet<FormletContext, UIElement, string> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let e =
                match ft with
                | Element (:? InputTextElement as e)-> e
                | _                                 ->
                    InputTextElement(initialText)
            e.ChangeNotifier <- cl
            (FormletResult.Success e.Text), Element (e :> UIElement)

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

    let DateTime (initialDateTime : DateTime option) : Formlet<FormletContext, UIElement, DateTime> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let e =
                match ft with
                | Element (:? InputDateTimeElement as e)-> e
                | _                                 ->
                    InputDateTimeElement(initialDateTime)
            e.ChangeNotifier <- cl

            let dt = e.DateTime
            let c =
                match dt with
                | Some d    -> FormletResult.Success d
                | _         -> FormletResult<_>.FailWith "Select a date"    // TODO: Localization

            c, Element (e :> UIElement)

        FormletMonad.New eval

    let Option (initial : int) (options : (string * 'T) []) : Formlet<FormletContext, UIElement, 'T option> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let e =
                match ft with
                | Element (:? InputOptionElement<'T> as e)-> e
                | _                                 ->
                    InputOptionElement<'T> (initial, options)
            e.ChangeNotifier <- cl

            let option = e.SelectedOption
            let c =
                match option with
                | Some o    -> FormletResult.Success option
                | _         -> FormletResult<_>.New option [FormletFailure.New [] "Select an option"] // TODO: Localization

            c, Element (e :> UIElement)

        FormletMonad.New eval


module Enhance =

    let Many (initialCount : int) (f : Formlet<FormletContext, UIElement, 'T>) : Formlet<FormletContext, UIElement, 'T[]> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let me, adorners =
                match ft with
                | Many ((:? ManyElement as me), adorners)   ->
                    me, adorners
                | _                         ->
                    let me      = ManyElement (initialCount)
                    let adorners= me.ChildCollection
                    me, adorners

            let adapter = adorners :?> AdornersAdapter

            me.ChangeNotifier <- cl

            let c       = Array.zeroCreate adorners.Count
            let fs      = ResizeArray<_> ()

            let count   = adorners.Count
            for i = 0 to count - 1 do
                let ls, ift  = adorners.[i]
                let ic, nift    = f.Evaluate (fc, cl, ift)
                adapter.SetFormletTree (i, nift)
                c.[i] <- ic.Value
                fs.AddRange ic.Failures

            FormletResult.New c (fs |> Seq.toList), Many (me :> UIElement, me.ChildCollection)

        FormletMonad.New eval

    let WithLabel (l : string) (f : Formlet<FormletContext, UIElement, 'T>) : Formlet<FormletContext, UIElement, 'T> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let (le, list, ift) =
                match ft with
                | Adorner ((:? LabelElement as le), list, ft)   ->
                    le, list, ft
                | _                                             ->
                    let le  = LabelElement (100.)
                    let list= le.ChildCollection
                    le, list, Empty

            let c,nift = f.Evaluate (fc, cl, ift)
            le.Text <- l
            c.AddContext l, Adorner (le :> UIElement, list, nift)

        FormletMonad.New eval


    let WithErrorVisual (f : Formlet<FormletContext, UIElement, 'T>) : Formlet<FormletContext, UIElement, 'T> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let ift =
                match ft with
                | Modify (_,ft)  -> ft
                | _             -> Empty
            let c,nift  = f.Evaluate (fc,cl,ift)
            let apply   = if c.HasFailures then AppendErrorAdorner else RemoveErrorAdorner
            c, Modify (apply, nift)

        FormletMonad.New eval

    let WithLegend (l : string) (f : Formlet<FormletContext, UIElement, 'T>) : Formlet<FormletContext, UIElement, 'T> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let le, list, ift =
                match ft with
                | Adorner ((:? LegendElement as le), list, ft) ->
                    le, list, ft
                | _                         ->
                    let le  = LegendElement ()
                    let list= le.ChildCollection
                    le, list, Empty

            let c,nift = f.Evaluate (fc, cl, ift)
            le.Text <- l
            c.AddContext l, Adorner (le :> UIElement, list, nift)

        FormletMonad.New eval

    let WithErrorSummary (f : Formlet<FormletContext, UIElement, 'T>) : Formlet<FormletContext, UIElement, 'T> =
        let eval (fc,cl,ft : FormletTree<UIElement>) =
            let ese, list, ift =
                match ft with
                | Adorner ((:? ErrorSummaryElement as ese), list, ft) ->
                    ese, list, ft
                | _                         ->
                    let ese = ErrorSummaryElement ()
                    let list= ese.ChildCollection
                    ese, list, Empty

            let c,nift = f.Evaluate (fc, cl, ift)
            ese.Failures <- c.Failures
            c, Adorner (ese :> UIElement, list, nift)

        FormletMonad.New eval
