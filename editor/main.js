let memory = undefined;

let elements = [];

const readString = (memory, string) => {
  const bytes = new Uint8Array(memory);
  const view = new DataView(bytes.buffer, string);
  const ptr = view.getInt32(0, true);
  const len = view.getInt32(4, true);
  return new TextDecoder().decode(bytes.slice(ptr, ptr + len));
};

const imports = {
  document: {
    create_element: (tag) => {
      const string = readString(memory, tag);
      const element = document.createElement(string);
      document.body.appendChild(element);
      const index = elements.length;
      elements.push(element);
      return index;
    },
  },
  element: {
    inner_html: (element, tag) => {
      elements[element].innerHTML = readString(memory, tag);
    },
  },
  console: {
    log: (text) => {
      console.log(readString(memory, text));
    },
  },
};

const onload = async () => {
  const wabt = await WabtModule();
  const wat = await fetch("onload.wat");
  const text = await wat.text();
  const wasm = await wabt.parseWat("onload.wat", text);
  const { instance } = await WebAssembly.instantiate(
    wasm.toBinary({}).buffer,
    imports
  );
  memory = instance.exports.memory.buffer;
  instance.exports.onload();
};

onload();
