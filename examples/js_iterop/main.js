let memory = undefined;
let instance = undefined;

const elements = [];

const readString = (memory, string) => {
  const bytes = new Uint8Array(memory);
  const view = new DataView(bytes.buffer, string);
  const ptr = view.getInt32(0, true);
  const len = view.getInt32(4, true);
  return new TextDecoder().decode(bytes.slice(ptr, ptr + len));
};

const importObject = {
  document: {
    create_element: (tag) => {
      const string = readString(memory, tag);
      const element = document.createElement(string);
      const index = elements.length;
      elements.push(element);
      return index;
    },
    create_element_ns: (ns, tag) => {
      const nsString = readString(memory, ns);
      const tagString = readString(memory, tag);
      const element = document.createElementNS(nsString, tagString);
      const index = elements.length;
      elements.push(element);
      return index;
    },
  },
  element: {
    inner_html: (element, tag) => {
      elements[element].innerHTML = readString(memory, tag);
      return element;
    },
    style: (element, property, value) => {
      const propertyString = readString(memory, property);
      const valueString = readString(memory, value);
      elements[element].style[propertyString] = valueString;
      return element;
    },
    set_attribute: (element, attribute, value) => {
      const attributeString = readString(memory, attribute);
      const valueString = readString(memory, value);
      elements[element].setAttribute(attributeString, valueString);
      return element;
    },
    append_child: (parent, child) => {
      elements[parent].appendChild(elements[child]);
      return parent;
    },
    add_event_listener: (element, event, callback) => {
      const eventString = readString(memory, event);
      const callbackString = readString(memory, callback);
      elements[element].addEventListener(eventString, () => {
        instance.exports[callbackString](element);
      });
      return element;
    },
  },
  console: {
    log: (text) => {
      console.log(readString(memory, text));
    },
  },
};

const onload = async () => {
  const result = await WebAssembly.instantiateStreaming(
    fetch("onload.wasm"),
    importObject,
  );
  instance = result.instance;
  memory = instance.exports.memory.buffer;
  document.body.appendChild(elements[instance.exports.onload()]);
};

onload();
