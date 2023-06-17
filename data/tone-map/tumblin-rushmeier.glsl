uniform sampler2D luminance_map;
uniform float ld_map, c_max;

vec3 tone_map(vec3 color){
  float l_in = color_luminance(color);
  float l_avg = texture(luminance_map, uv, 10.0).r;
  float log_lrw = log10(l_avg) + 0.84;
  float alpha_rw = 0.4 * log_lrw + 2.92;
  float beta_rw = -0.4 * log_lrw * log_lrw - 2.584 * log_lrw + 2.0208;
  float lwd = ldmax / sqrt(cmax);
  float log_ld = log10(lwd) + 0.84;
  float alpha_d = 0.4 * log_ld + 2.92;
  float beta_d = -0.4 * log_ld * log_ld - 2.584 * log_ld + 2.0208;
  float l_out = pow(l_in, alpha_rw / alpha_d) / ld_max * pow(10.0, (beta_rw - beta_d) / alpha_d) - (1.0 / c_max);
  return color / l_in * l_out;
}
