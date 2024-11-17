#section FRAGMENT_SHADER
uniform float intensity = 1.0;

vec4 post_process(sampler2D previous_pass, vec2 uv){
  float n = -intensity;
  float c = intensity * 4 + 1;

  return n * textureOffset(previous_pass, uv, ivec2(0, +1))
    + n * textureOffset(previous_pass, uv, ivec2(+1, 0))
    + c * texture(previous_pass, uv)
    + n * textureOffset(previous_pass, uv, ivec2(0, -1))
    + n * textureOffset(previous_pass, uv, ivec2(-1, 0));
}
