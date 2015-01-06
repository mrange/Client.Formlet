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


/// IFormletContext allows adaptations to provide Flowlet-wide Context
///  Show displays a formlet
type IFlowletContext<'FormletContext, 'Element when 'FormletContext : not struct and 'FormletContext :> IFormletContext and 'Element : not struct> =
    abstract Show       : ('T -> unit)*int*Formlet<'FormletContext, 'Element, 'T> -> unit

/// Flowlet is basically a continuation monad
///  Note: As the flowlet might go back and forth between formlet pages flowlets shouldn't have side-effects
type Flowlet<'Context, 'FormletContext, 'Element, 'T when 'Context : not struct and 'Context :> IFlowletContext<'FormletContext, 'Element> and 'FormletContext : not struct and 'FormletContext :> IFormletContext and 'Element : not struct> =
    {
        Continuation : 'Context*int*('T*int -> unit) -> unit
    }
    static member New continuation = { Continuation = continuation }

    member this.Continue    = this.Continuation

module FlowletMonad =
    let inline New continuation : Flowlet<'Context, 'FormletContext, 'Element, 'T> = Flowlet<_,_,_,_>.New continuation

    let Zero () : Flowlet<'Context, 'FormletContext, 'Element, 'T> =
        let cont (ctx, pageNo, success) = success (EmptyValueProvider.GetEmptyValue<_> (), pageNo)

        New cont

    let Return (v : 'T) : Flowlet<'Context, 'FormletContext, 'Element, 'T> =
        let cont (ctx, pageNo, success) = success (v,pageNo)

        New cont

    let ReturnFrom (f : Flowlet<'Context, 'FormletContext, 'Element, 'T>) : Flowlet<'Context, 'FormletContext, 'Element, 'T> = f

    let Delay (df : unit -> Flowlet<'Context, 'FormletContext, 'Element, 'T>) : Flowlet<'Context, 'FormletContext, 'Element, 'T> =
        let cont (ctx, pageNo, success) =
            let f = df ()
            f.Continue (ctx, pageNo, success)

        New cont

    let Bind (f1 : Flowlet<'Context, 'FormletContext, 'Element, 'T1>) (u2 : 'T1 -> Flowlet<'Context, 'FormletContext, 'Element, 'T2>) : Flowlet<'Context, 'FormletContext, 'Element, 'T2> =
        let cont (ctx, pageNo, success) =
            let succ (v1, pno1) =
                let f2 = u2 v1
                f2.Continue (ctx, pno1, success)
                ()
            f1.Continue (ctx, pageNo, succ)

        New cont

    let Run (f : Flowlet<'Context, 'FormletContext, 'Element, 'T>) : Flowlet<'Context, 'FormletContext, 'Element, 'T> = f

    [<Sealed>]
    type FlowletBuilder () =
        member this.Return      x       = Return        x
        member this.Bind        (x, f)  = Bind          x f
        member this.Delay       f       = Delay         f
        member this.ReturnFrom  f       = ReturnFrom    f
        member this.Run         f       = Run           f
        member this.Zero        ()      = Zero          ()


module Flowlet =
    /// Map maps the value of a Flowlet from one type into another type
    let Map (m : 'T -> 'U) (f : Flowlet<'Context, 'FormletContext, 'Element, 'T>) : Flowlet<'Context, 'FormletContext, 'Element, 'U> =
        let cont (ctx : 'Context, pageNo, success) =
            let succ (v : 'T, pno : int)  =
                success ((m v),pno)
            f.Continue (ctx, pageNo, succ)

        FlowletMonad.New cont

    /// Shows a Formlet as a page
    let Show (ff : int -> Formlet<'FormletContext, 'Element, 'T>) : Flowlet<'Context, 'FormletContext, 'Element, 'T> =
        let cont (ctx : 'Context, pageNo, success) =
            let f = ff pageNo
            let succ v = success (v, pageNo + 1)
            ctx.Show (succ, pageNo, f)

        FlowletMonad.New cont


[<AutoOpen>]
module FlowletAutoOpen =
    let flowlet = FlowletMonad.FlowletBuilder ()
