package hyperfiddle;
import hyperfiddle.Meta;
using Lambda;
using hyperfiddle.Meta.X;

@:expose @:publicFields class Origin {                       // public API singleton
  static var main : Flow;
  static function get() return if(main != null) main else main = new Flow();
  static function all(f) get().all(f);

  static var onError : Error -> Void = (error) -> trace(error);
  static var executor : Dynamic;

  static function input<A>(?f) {
    return new Input<A>(get(), new Push(From({
      var end = null;
      { on: () -> if(f != null) end = f(),
        off: () -> if(end != null) {end(); end = null;} } })));
  }

  static function on<A>(v : View<A>, f : A -> Void) {
    var out =  new Output(get(), new Push([v.node], Into(f)));
    out.activate();
    return out;
  }

  static function apply(ns : Array<View<Dynamic>>, f : Dynamic) {
    return new View(get(), new Push([for(n in ns) n.node], ApplyN(f)));
  }

  static function applyAsync(ns : Array<View<Dynamic>>, f : Dynamic) {
    return new View(get(), new Push([for(n in ns) n.node], ApplyAsync(f)));
  }

  static function pure<A>(a: A) {
    return new View(get(), new Push(Const(a)));
  }

  static function bind<A>(n: View<Dynamic>, f: Dynamic -> View<A>) {
    return new View(get(), new Push([n.node], Bind(f)));
  }

  static function getNodeDef(){return NodeDef;}
}

@:publicFields class View<A> {
  var F : Flow;
  var node : Push<A>;
  function new(f, n) {F = f; node = n;}
  function setName(name){ node.setName(name); }
}

@:publicFields class Input<A> extends View<A> {
  function put(a : A) {
    (cast Origin.executor)(node, a);
    F.resume(node, Val(a));
  }
  function end() {F.resume(node, End);}
}

@:publicFields class Output<A> extends View<A> {
  function activate() {F.activate(node);}
  function off() {F.resume(node, End);} // detach?
}

@:expose // @:expose is not implemented for enums in JS, circumevented by Origin.getNodeDef.
enum NodeDef<T> {       // GT the NodeDef values essentially define a live AST of what should be done
  Const<A>(a: A) : NodeDef<A>;
  From<A>(source : {on : () -> Void, off : () -> Void}) : NodeDef<A>;
  Into<A>(f : A -> Void);                                   // terminal node
  ApplyN<A>(f : Array<Dynamic> -> A) : NodeDef<A>;
  ApplyAsync<A>(f : Array<Dynamic> -> (A -> Void) -> (A -> Void) -> Void) : NodeDef<A>;
  Bind<A>(f : Dynamic -> View<A>) : NodeDef<A>;
}

enum Action<A> {
  Val(a : A);
  Error(e : A);
  End;
}

enum Maybe<A> {
  Just(a : A);
  Nothing;
}

@:nullSafety(Loose)
@:publicFields class Flow {                                 // singleton
  static var count = 0;
  static function id() {return ++count;}

  var lock : Bool = false;
  var frame : Int = 0;                                    // ?
  var queue : Array<Array<Push<Dynamic>>> = [];             // Many pushes, grouped by rank

  function new() {}

  function resume<A>(node : Push<A>, a : Action<A>) {
    node.resume(a);
    var next = node.unlink();
    if(next != null) add(next);
    pump();
  }

  function all(f : Void -> Void) {                          // batch put in one frame
    lock = true;
    var e : Null<Error> = null;
    try f() catch(x : Error) e = x;
    lock = false;
    pump();
    onError(e);
  }

  function add(node : Push<Dynamic>) {      // queue
    trace("add "+ node.id + " rank " +node.rank);
    if(node.queued) return;
    while(queue.length <= node.rank) queue.push([]); // priority queue
    queue[node.rank].push(node);
    node.queued = true;
  }

  function pump() {                          // Run queue until empty
    trace("pump");
    if(lock) { trace("pump lock=true"); return; }
    lock = true;

    frame++;

    var e : Null<Error> = null;
    try { // Flow owns the queue. Flow chooses what order the nodes will run in and runs them in that order.
      var rank = 0;                         // for ordering dependencies

      while(rank < queue.length) {
        for(node in queue[rank])
          // can node.run return its plan?
          node.run(this);                   // compute the node and plan what happens next

        // loop through the new plan, either queing or running now
        for(node in queue[rank]) {
          var next = node.unlink();
          while(next != null) {
            if(next.rank == rank)
              next.run(this);
            else if(next.rank > rank)
              add(next);
            next = next.unlink();
          }
        }

        for(node in queue[rank])
          clear(node);                      // mark join nodes as not-ok, but why?

        queue[rank].resize(0);              // empty this layer of queue

        rank++;
      }
    }
    catch(x : Error) e = x;
    lock = false;
    onError(e);
  }

  function into<A>(n : Push<A>, f : Null<A> -> Void, val : Null<A>) {
    if(val != null) n.val = Just(val);
    f(val);
  }

  function onError(?e) {
    if(e != null) {
      queue.resize(0); // Reset to clean state so user can recover
      Origin.onError(e);
    }
  }

  function clear(b : Push<Dynamic>) {
    if(!b.queued) return;                           // ?
    b.queued = false;

//    if(!b.outputs.opt().exists(c -> c.joins()))
//      b.val = Nothing;                                // mark not ok, but why?

    for(a in b.inputs) clear(a);
  }

  function activate(b : Push<Dynamic>) {
    trace("activate ", b.def);
    if(!b.active()) return;
    for(a in b.ups) {
      attach(a, b);
    }
  }

  function attach(a : Push<Dynamic>, b : Push<Dynamic>) {
    trace("attach ", a.def, b.def);
    var firstTime = a.outputs.length > 0;

    if (!a.def.match(Bind(_))) {
      b.inputs.push(a);
      if(!a.outputs.has(b)) a.outputs.push(b);
    }

    computeRank(a);

    switch (a.def) {
      case Bind(_): {
        var cross = a;
        cross.z = cast b; // save output node for rewiring once q is known
        //activate(cross); // on next control push, cross will activate q
        // no inputs/outputs
        // var aa = a.ups[0]
      }
      case Const(v): {
        // if we're in an on, flow the first time?
        // if we're in a push, don't interrupt?
        /*if (!firstTime)*/ resume(a, Val(v)); // the lock does the right thing here
      }
      case From(source):
        if (!firstTime) if(source.on != null) source.on();
      default:
    }

    activate(a); // activate before resume ???
  }

  function computeRank(b : Push<Dynamic>){
    b.rank = [for(a in b.inputs) a.rank].fold((a : Int, b :Int) -> cast (Math.max(a + 0., b + 0.)), b.rank);
    if(b.joins()) b.rank++;
    for(c in b.outputs){
      computeRank(c);
    }
  }

  function detach(b : Push<Dynamic>, c : Push<Dynamic>) {
    trace("detach ", b.def, c.def);
    var x = b.outputs.remove(c);
    var xx = c.inputs.remove(b);
    if(x) {
      if(b.outputs.nil())
        switch(b.def) {
          case From(source): if(source.off != null) source.off();
          default: for (a in b.inputs) detach(a, b); // traverse backwards until we find the source
        }
    }
  }
}

@:nullSafety(Off)
@:publicFields class Push<A> {
  var def : NodeDef<A>;

  var ups     : Array<Push<Dynamic>> = []; // points towards a static source, possibly off
  var inputs  : Array<Push<Dynamic>> = []; // dynamic value back-refs for when node runs
  var outputs : Array<Push<Dynamic>> = []; // dynamic value forward-refs for push when active

  // bind-only state, needed for dynamic rewiring
  var q : Null<Push<Dynamic>>; // bind's choice
  var z : Null<Push<Dynamic>>; // bind's output

  var id : Int = Flow.id();
  var name : Null<Dynamic>;
  var rank : Int = 0;
  var frame : Int = 0;
  var queued : Bool = false;

  var val : Maybe<A> = Nothing;
  var error : Null<Dynamic>;
  var ended : Bool = false;

  var next : Null<Push<Dynamic>>;
  var prev : Null<Push<Dynamic>>;

  function new(?name, ?as : Array<Push<Dynamic>>, d) {
    def = d;
    if(ns != null) on = ns.copy(); // weakref?
  }

  function setName(name){
    this.name = name;
  }

  function toString() return 'Push($id, $rank, $def)';

  function ok()     return val != Nothing && !ended; // ready
  function active() return !ended && (outputs.length > 0 || z != null || def.match(Into(_)));
  function joins()  return inputs.length > 1;

  function extract(a : Maybe<A>) : Null<A>{
    return switch(a){
      case Just(a) : a;
      case Nothing : null;
    }
  }

  function run(F : Flow) {
    if(frame == F.frame) return;                    // already ran this node?
    frame = F.frame;                                // Mark ran

    if(!active()) return;                           // Skip the work, nobody is listening
    trace("run ", [for (a in inputs) a.ok()], def);

    switch(def) {
      case From(_):  {}
      case Into(_), ApplyN(_), ApplyAsync(_), Const(_), Bind(_):
        // bind changes the ready rules. check on2 not on on what circumstance?
        if(inputs.foreach(a -> a.ok())) // .ready
          switch(def) {
            case Into(f): {
              var a : Dynamic = inputs[0].val;
              F.into(this, cast f, extract(cast a));
            }
            case Const(x): resume(Val(x));
            case ApplyN(f):
              try{
                var as = [for(a in on) extract(cast a.val)]; //trace(as);
                var b = (cast Origin.executor)(this, f, as); //trace(b);
                resume(Val(b));
              } catch (e : Dynamic) {
                //trace("run e caught, resume with e ", e);
                resume(Error(e));
              }
            case ApplyAsync(f):
              (cast Origin.executor)(this, f, [for(n in on) extract(cast n.val)],
                       err -> F.resume(this, Error(err)),
                       v   -> F.resume(this, Val(v)));

            case Bind(f): {
              // Rewire the graph by redirecting future mb pushes to our output
              if (z == null) trace("?? inactive bind");
              var cross : Push<Dynamic> = this;
              if (q != null) F.detach(q, z); // todo skip if same
              var a : Dynamic = cast extract(cast inputs[0].val); // control
              trace("bind a", a);
              try {
                var mb : Dynamic = (cast Origin.executor)(name, f, a); // user crash
                var mb : View<Dynamic> = cast mb; // user type error
                q = mb.node;
                trace("bind q", q);
                q.rank = rank+1; // second DAG starts at next rank
                F.attach(q, z);
              }
              catch (e : Dynamic) {
                trace("bind user crash or type error ", e);
                // Must recover, otherwise the nodes are still queued & flow is corrupt
                F.resume(this, Error(e)); // also must clear queue on error
              }
            }

            default: {}
          }
        else if(inputs.exists(n -> n.ended))
          resume(End);
        try{
          for(a in inputs)
            if(a.error != null) {
              trace("run error ", a);
              throw a.error; // break
            }
        }
        catch(e : Dynamic){
          trace("run error, resume with error ", e);
          resume(Error(e));
        }
     }
  }

  function resume(a : Action<A>) {
    trace("resume ", a, def);
    switch(a) {
      case Val(v):   {val = Just(v); error = null;} // allow recovery e.g. at REPL
      case Error(e): error = e;
      case End:      ended = true;
    }
    plan();
  }

  function plan() {
    trace("plan ", def, outputs);
    if(outputs.length == 0) return;
    outputs.sort((a, b) -> a.rank - b.rank);
    var a : Push<Dynamic> = this;
    for(b in outputs) {
      if(b.queued) continue;
      a.link(b);
      a = b;
    }
  }

  function link(a : Push<Dynamic>) {
    if(a == next) return;
    a.unlink();
    a.next = next;
    a.prev = this;
    next = a;
  }

  function unlink() { // Awkwardly shifts the list, but returning next node not removed node
    if(prev != null) prev.next = next;
    if(next != null) next.prev = prev;
    var b = next;
    prev = null; next = null;
    return b;
  }
}
