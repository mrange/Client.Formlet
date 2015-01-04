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

type IFlowletContext =
    abstract Show   : Formlet<'Context, 'Element, 'T>*('T -> unit) -> unit

type FlowletCompletion =
    | Cancelled
    | Completed

type Flowlet<'Context, 'Element, 'T when 'Context : not struct and 'Context :> IFlowletContext and 'Element : not struct> =
    {
        Continuation : 'Context*('T -> unit)*(FlowletCompletion->unit) -> unit
    }
    static member New continuation = { Continuation = continuation }

module FlowletMonad =
    let inline New continuation : Flowlet<'Context, 'Element, 'T> = Flowlet<_,_,_>.New continuation

    let Zero () : Flowlet<'Context, 'Element, 'T> =
        let cont (ctx, success, completion) = success (EmptyValueProvider.GetEmptyValue<_> ())

        New cont

    let Return (v : 'T) : Flowlet<'Context, 'Element, 'T> =
        let cont (ctx, success, completion) = success v

        New cont

    let ReturnFrom (f : Flowlet<'Context, 'Element, 'T>) : Flowlet<'Context, 'Element, 'T> = f

    let Delay (df : unit -> Flowlet<'Context, 'Element, 'T>) : Flowlet<'Context, 'Element, 'T> =
        let cont (ctx, success, completion) = 
            let f = df ()
            f.Continuation (ctx, success, completion)

        New cont

    let Bind (f1 : Flowlet<'Context, 'Element, 'T1>) (u2 : 'T1 -> Flowlet<'Context, 'Element, 'T2>) : Flowlet<'Context, 'Element, 'T2> =
        let cont (ctx, success, completion) =
            let succ v1 = 
                let f2 = u2 v1
                f2.Continuation (ctx, success, completion)
                ()
            f1.Continuation (ctx, succ, completion)

        New cont

    let Run (f : Flowlet<'Context, 'Element, 'T>) : Flowlet<'Context, 'Element, 'T> = f

    [<Sealed>]
    type FlowletBuilder () =
        member this.Return      x       = Return        x
        member this.Bind        (x, f)  = Bind          x f
        member this.Delay       f       = Delay         f
        member this.ReturnFrom  f       = ReturnFrom    f
        member this.Run         f       = Run           f
        member this.Zero        ()      = Zero          ()


module Flowlet =
    let Show (f : Formlet<'FormletContext, 'Element, 'T>) : Flowlet<'Context, 'Element, 'T> = 
        let cont (ctx : IFlowletContext , success, completion) =
            ctx.Show (f, success)

        FlowletMonad.New cont

[<AutoOpen>]
module FlowletAutoOpen =
    let flowlet = FlowletMonad.FlowletBuilder ()
