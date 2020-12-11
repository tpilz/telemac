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
  unsigned long int len_val_max = 0;
  unsigned long int len_key_max = 0;
  int p;
  int h = 0;
  std::string si, sp, st;
  std::vector<int> len_excess_i;

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
    for (int i = 0; i < len_excess_i.size(); ++i) {
      do {
        p = len_excess_i[i] + h + 1;
        st = value[p-1]; // value string to be split
        si = st.substr(0, 72 - len_key_max); // first part of value string
        sp = st.substr(72 - len_key_max, st.length() - si.length()); // second part
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
