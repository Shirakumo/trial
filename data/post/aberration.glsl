// Simplified from https://www.shadertoy.com/view/XssGz8
#section FRAGMENT_SHADER
uniform float offset = 3.0;

vec3 spectrum_offset(float t){
  float t0 = 3.0 * t - 1.5;
  return clamp(vec3(-t0, 1.0-abs(t0), t0), 0.0, 1.0);
}

vec2 brown_conrady_distortion(vec2 uv, float dist){
  uv = uv * 2.0 - 1.0;
  float barrelDistortion1 = 0.1 * dist;
  float barrelDistortion2 = -0.025 * dist;

  float r2 = dot(uv,uv);
  uv *= 1.0 + barrelDistortion1 * r2 + barrelDistortion2 * r2 * r2;
  return uv * 0.5 + 0.5;
}

vec2 remap(vec2 t, vec2 a, vec2 b){
  return clamp((t - a) / (b - a), 0.0, 1.0);
}

vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec2 max_distort = vec2(offset*100) / textureSize(previous_pass, 0).xy;
  vec2 min_distort = 0.5 * max_distort;
  vec2 oversiz = brown_conrady_distortion(vec2(1.0), min_distort.x);
  vec2 cuv = remap(uv, 1.0-oversiz, oversiz);
    
  const int num_iter = 7;
  const float stepsiz = 1.0 / (float(num_iter)-1.0);
  float t = stepsiz;

  vec3 sumcol = vec3(0.0);
  vec3 sumw = vec3(0.0);
  for(int i=0; i<num_iter; ++i){
    vec3 w = spectrum_offset(t);
    sumw += w;
    vec2 uvd = brown_conrady_distortion(cuv, mix(min_distort.x, max_distort.x, t));
    sumcol += w * texture(previous_pass, uvd).rgb;
    t += stepsiz;
  }
  sumcol.rgb /= sumw;
  return vec4(sumcol.rgb, 1.0);
}
