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

const promises = {}

//Server API
window.Server = class {
   
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
      promises[uid] = promise;
      console.log('Asking expr');
      console.log(expr);
      this.socket.send('WLJSIOPromise["'+uid+'", ""]['+expr+']');
  
      return promise.promise 
    };
    //fire event on the secondary kernel (your working area) (no reply)
    emitt(uid, data, type = 'Default') {
      this.socket.send('EventFire["'+uid+'", "'+type+'", '+data+']');
    };

    _emitt(uid, data, type) {
      //unescaped version
      console.log({uid:uid, data:data, type:type});
      this.socket.send('EventFire["'+uid+'", '+type+', '+data+']');
    };    
  
    send(expr) {
      this.socket.send(expr);
    };
  
    getSymbol(expr) {
      const uid = uuidv4();
  
      const promise = new Deferred();
      promises[uid] = promise;
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
    //console.log("update");
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
    if (args[1] == '$Failed') {
      promises[uid].reject(args[1]);
    } else {
      promises[uid].resolve(args[1]);
    }
    console.log('promise resolved! : ' + uid);
    delete promises[uid];
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

const bjtag = decodeURIComponent('%3Cscript%20type%3D%22module%22%3E');
const ejtsg = decodeURIComponent('%3C%2Fscript%3E');

core.WLXEmbed = async (args, env) => {
    const options = await core._getRules(args, env);
    const html = await interpretate(args[0], env);
    if (Array.isArray(html)) {
        //console.log(JSON.stringify(html));
        const jsdata = html.pop();
        env.element.innerHTML = html.join('');
        
        
        const script = document.createElement('script');
        script.type = "module";
        script.textContent = jsdata.replaceAll(bjtag, '').replaceAll(ejtsg, '');
        env.element.appendChild(script);
        return;
    }
    env.element.innerHTML = html;
    if ('JS' in options) {
        const jsdata = options.JS.replaceAll(bjtag, '').replaceAll(ejtsg, '');
        const script = document.createElement('script');
        script.textContent = jsdata;
        env.element.appendChild(script);
    }
}   