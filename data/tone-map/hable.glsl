#section FRAGMENT_SHADER
uniform float a,b,c,d,e,f,w;
uniform float exposure_bias = 2.0;

vec3 filmic_curve(vec3 x){
  return ((x*(a*x+c*b)+d*e)/(x*(a*x+b)+d*f))-e/f;
}

vec3 tone_map(vec3 color){
  vec3 curr = exposure_bias * filmic_curve(color);
  vec3 white_scale = 1.0 / filmic_curve(vec3(w));
  return curr * white_scale;
}
