# section FRAGMENT_SHADER
uniform sampler2D displacement_map;
uniform float intensity = 1.0;

vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec2 displacement = texture(displacement_map, uv).rg;
  return texture(previous_pass, uv+displacement*intensity);
}
