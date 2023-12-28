function jsonStringifyRecursive(obj) {
    const cache = new Set();
    return JSON.stringify(obj, (key, value) => {
        if (typeof value === 'object' && value !== null) {
            if (cache.has(value)) {
                // Circular reference found, discard key
                return;
            }
            // Store value in our collection
            cache.add(value);
        }
        return value;
    }, 4);
}

//default plug for a server object
window.server = undefined;

interpretate.anonymous = async (d, org) => {
    //TODO Check if it set delayed or set... if set, then one need only to cache it
    console.warn('Anonimous symbol');  
  
    let name;
    //check it is a plain symbol
    if (d instanceof Array) {
      console.error('stack call: ');
      console.error(jsonStringifyRecursive(org.global.stack));
      throw('downvalues/subvalues are not supported in general. Error at '+d[0]);
    } else {
      name = d;   //symbol
    }
  
    let data;
    let packed = false;
  
    //request it from the server
    console.warn('sending request to a server... for'+name);
    data = await server.kernel.getSymbol(name); //get the data
    console.log('got');
    console.log(data);
    
    //check for strings 
    let symbolQ = typeof data === 'string';
  
    if (symbolQ) {
      if (data.charAt(0) == "'") symbolQ = false;
      if (isNumeric(data)) symbolQ = false;
    }
  
    //if it is a shit
    if ((symbolQ && !(data in core)) || typeof data == 'undefined') {
      console.log('checking... '+name);
      console.log('recived data..');
      console.log(jsonStringifyRecursive(data));
      console.error('stack call: ');
      console.error(jsonStringifyRecursive(org.global.stack));
      throw('received symbol '+data+' is not defined in any contextes and packing'); 
    }
  
    //if it is OK
  
    core[name] = async (args, env) => {
      console.log('calling our symbol...');
      //evaluate in the context
      const data = await interpretate(core[name].data, env);
  
      if (env.root && !env.novirtual) core[name].instances[env.root.uid] = env.root; //if it was evaluated insdide the container, then, add it to the tracking list
      //if (env.hold) return ['JSObject', core[name].data];
  
      return data;
    }
  
    core[name].update = async (args, env) => {
      //evaluate in the context
   
      const data = await interpretate(core[name].data, env);
      //if (env.hold) return ['JSObject', data];
      return data;
    }  
  
    core[name].destroy = async (args, env) => {
  
      delete core[name].instances[env.root.uid];
      console.warn(env.root.uid + ' was destroyed')
      console.warn('external symbol was destoryed');
    }  
  
    core[name].data = data; //get the data
  
    server.kernel.addTracker(name);
    server.kernel.trackedSymbols[name] = true;
  
    core[name].virtual = true;
    core[name].instances = {};
  
    //interpretate it AGAIN!
    return interpretate(d, org);
  }