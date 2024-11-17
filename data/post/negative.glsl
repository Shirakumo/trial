#section FRAGMENT_SHADER
vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec4 color = texture(previous_pass, uv);
  return vec4(1.0-color.x, 1.0-color.y, 1.0-color.z, color.w);
}
