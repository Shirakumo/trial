#section FRAGMENT_SHADER

float color_luminance(vec3 color){
  return dot(color, vec3(0.299, 0.587, 0.114));
}

vec4 post_process(sampler2D previous_pass, vec2 uv){
  return color_luminance(texture(previous_pass, uv).rgb);
}
