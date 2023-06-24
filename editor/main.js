let memory = undefined;

let elements = [];

const imports = {
  document: {
    create_element: (tag) => {
      const bytes = new Uint8Array(memory);
      const view = new DataView(bytes.buffer, tag);
      const ptr = view.getInt32(0, true);
      const len = view.getInt32(4, true);
      const string = new TextDecoder().decode(bytes.slice(ptr, ptr + len));
      const element = document.createElement(string);
      document.body.appendChild(element);
      const index = elements.length;
      elements.push(element);
      return index;
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
