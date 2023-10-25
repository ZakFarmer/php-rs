import("../pkg/index.js")
  .then((wasm) => {
    wasm.init_state();

    const inputElement = document.getElementById("input");
    const outputElement = document.getElementById("output");

    outputElement.innerHTML += `> php-rs interpreter 0.1.0\n`;
    outputElement.innerHTML += `Loaded WASM bundle.\n\n`;

    inputElement.addEventListener("keydown", (event) => {
      if (event.key === "Enter" && !event.shiftKey) {
        event.preventDefault();

        const input = inputElement.value;
        const output = wasm.parse(input);

        outputElement.innerHTML += `> ${input}\n${output}\n`;
        inputElement.value = ""; // Clear the input
      }
    });
  })
  .catch(console.error);
