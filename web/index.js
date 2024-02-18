import * as one_stack from "one_stack";

const d = one_stack.Driver.new();
console.log(d);

$("input#run").on("click", function(evt) {
  const code = $("textarea#code").val();

  d.run_string(code);

  $("#stack").html(d.render_stack().replaceAll("\n", "<br>"));
  if (d.has_error()) {
    $("div#status").html("Error: " + d.error_string());
  } else {
    $("div#status").html("OK");
  }
});
