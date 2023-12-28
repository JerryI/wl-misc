core.SlientPing = () => {
    console.log('Ppspsp... server is there');
}

class fakeSocket {
  
  q = []

  isFakeSocket = true
  
  send = (expr) => {
    console.warn('No connection to a kernel... keeping in a pocket');
    this.q.push(expr)
  }

  constructor() {

  }
}  
//Server API
class Server {
    promises = {};
    socket = new fakeSocket();  
  
    trackedSymbols = {};
    name = 'Unknown';
  
    kernel;
  
    constructor(name = "Unknown") {
      console.warn('Server was constructed with name '+name);
      this.name = name;
    }
  
    init(opts) {
      //opts: socket
      //opts: kernel
  
      if (!opts.socket) throw('Socket is not provided!');
      
      this.kernel = this;

      const self = this;
  
      if (opts.kernel) {
        console.log('kernel object was provided');
        this.kernel = opts.kernel;
      }
  
      if (this.socket.isFakeSocket) {
        console.warn('Sending all quered messages');

        this.socket.q.forEach((message)=>{
          opts.socket.send(message);
        });
      } 

      this.socket = opts.socket;

      
    }
  
    //evaluate something on the master kernel and make a promise for the reply
    ask(expr) {
      const uid = uuidv4();
  
      const promise = new Deferred();
      this.promises[uid] = promise;
  
      this.socket.send('WLJSIOPromise["'+uid+'", ""]['+expr+']');
  
      return promise.promise 
    };
    //fire event on the secondary kernel (your working area) (no reply)
    emitt(uid, data, type = 'Default') {
      this.socket.send('EventFire["'+uid+'", "'+type+'", '+data+']');
    };
  
    send(expr) {
      this.socket.send(expr);
    };
  
    getSymbol(expr) {
      const uid = uuidv4();
  
      const promise = new Deferred();
      this.promises[uid] = promise;
      //not implemented
      //console.error('askKernel is not implemented');
      //console.log('NotebookPromiseKernel["'+uid+'", ""][Hold['+expr+']]');
      this.socket.send('WLJSIOGetSymbol["'+uid+'", ""][Hold['+expr+']]');
  
      return promise.promise     
    }
  
    addTracker(name) {
      console.warn('added tracker for '+name);
      this.socket.send('WLJSIOAddTracking['+name+']')
    }
}

core.WLJSIOUpdateSymbol = (args, env) => {
    const name = interpretate(args[0], env);
    console.log("update");
    //update
    core[name].data = args[1];

    //console.log('instance list');
    //console.log(core[name].instances);

    for (const inst of Object.values(core[name].instances)) {
        inst.update();
    };
}

core.WLJSIOPromiseResolve = (args, env) => {
    const uid = interpretate(args[0], env);
    console.log('promise resolved! : ' + uid);
    server.kernel.promises[uid].resolve(args[1]);
    delete server.kernel.promises[uid];
}

core.FireEvent = function(args, env) {
    const key = interpretate(args[0], env);
    const data = interpretate(args[1], env);

    server.kernel.emitt(key, data);
}

core.KernelFire = function(args, env) {
    const data = interpretate(args[0], env);

    server.talkKernel(data);
}

core.KernelEvaluate = function(args, env) {
    const data = interpretate(args[0], env);

    server.askKernel(data);
}

core.TalkMaster = async(args, env) => {
    const data = await interpretate(args[0], env);
    const wrapper = await interpretate(args[1], env);
    server.send(wrapper + '["' + JSON.stringify(data).replace(/"/gm, "\\\"") + '"]');
}

core.TalkKernel = async(args, env) => {
    const data = await interpretate(args[0], env);
    const wrapper = await interpretate(args[1], env);
    server.kernel.send(wrapper + '["' + JSON.stringify(data).replace(/"/gm, "\\\"") + '"]');
}