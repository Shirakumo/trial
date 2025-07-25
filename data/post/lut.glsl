#section FRAGMENT_SHADER
uniform sampler3D lut;

vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec4 prev = texture(previous_pass, uv);
  vec3 mapped = texture(lut, prev.rgb).rgb;
  return vec4(mapped, prev.a);
}
