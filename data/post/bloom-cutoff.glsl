#section FRAGMENT_SHADER
uniform float threshold = 2.0;

float color_luminance(vec3 color){
  return dot(color, vec3(0.299, 0.587, 0.114));
}

vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec4 color = texture(previous_pass, uv);
  float luminance = color_luminance(color.xyz);
  color.rgb *= clamp((luminance/threshold)-0.5, 0.0, 1.0);
  return color;
}
