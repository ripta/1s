import * as one_stack from "one_stack";

const d = one_stack.Driver.new();
console.log(d);

$("input#run").on("click", function(evt) {
  const code = $("textarea#code").val();
  console.log("Code:", code);
  console.log("Run:", d.run_string(code));
  if (d.has_error()) {
    console.log("Err:", d.error_string());
  } else {
    console.log("OK");
  }
});
