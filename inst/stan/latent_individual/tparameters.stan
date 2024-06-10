vector<lower = 0>[N] pwindow;
vector<lower = 0>[N] swindow;
swindow = to_vector(vreal3) .* swindow_raw;
pwindow[noverlap] = to_vector(vreal2[noverlap]) .* pwindow_raw[noverlap];
if (wN) {
  pwindow[woverlap] = swindow[woverlap] .* pwindow_raw[woverlap];
}
