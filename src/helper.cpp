#include <Rcpp.h>
using namespace Rcpp;

// df: data.frame with 'key' (steering poarameter) and 'value', both as character vector.
// sep_len: length of separator between key and value in the steering file.
// output: Updated data.frame (as input).
// [[Rcpp::export]]
DataFrame cas_lineadapt(DataFrame df, int sep_len) {
  std::vector< std::string > key = df["key"];
  std::vector< std::string > value = df["value"];
  int n = df.nrows();
  IntegerVector len(n), len_key;
  std::size_t len_val_max = 0;
  std::size_t len_key_max = 0;
  int p;
  int h = 0;
  std::string si, sp, st;
  std::vector<int> len_excess_i;
  // regex via R base function grepl
  Environment base("package:base");
  Function grepl = base["grepl"];
  LogicalVector grp(1);

  // get string lengths
  for (int i = 0; i < n; ++i) {
    len_val_max = std::max(value[i].length(), len_val_max);
    len_key_max = std::max(key[i].length(), len_key_max);
  }
  len_key_max += sep_len;

  // value lengths must not be longer than 144 characters
  if (len_val_max > 144)
    stop("There are steering parameter values with more than 144 characters which is not allowed!");

  // determine excessively long line (> 72 characters)
  for (int i = 0; i < n; ++i)
    if ( (len_key_max + value[i].length()) > 72) len_excess_i.push_back(i);

  // treatment of excessive lengths
  if (len_excess_i.size() > 0) {
    for (std::size_t i = 0; i < len_excess_i.size(); ++i) {
      do {
        p = len_excess_i[i] + h + 1;
        st = value[p-1]; // value string to be split
        // if there is a slash only one line possible
        grp = grepl("/", st);
        if (grp[0]) {
          si = ""; // first part empty
          sp = st; // second part: whole string in new line
          if (sp.length() > 72)
            stop("Steering parameter values containing a slash '/' character must not be longer than 72 characters!");
        } else {
          si = st.substr(0, 72 - len_key_max); // first part of value string
          sp = st.substr(72 - len_key_max, st.length() - si.length()); // second part
        }
        // update key and value
        key.insert(key.begin() + p, "");
        value[p-1] = si;
        value.insert(value.begin() + p, sp);
        h++;
      } while (sp.length() > 72); // again if new line is still too long
    }
  }

  return DataFrame::create(_["key"] = key,
                           _["value"] = value,
                           _["stringsAsFactors"] = false);
}
