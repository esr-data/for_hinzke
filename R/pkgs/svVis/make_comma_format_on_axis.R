
make_comma_format_on_axis <- function() {
  function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}
