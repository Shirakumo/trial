#section FRAGMENT_SHADER
uniform float intensity = 1.0;

vec4 post_process(sampler2D previous_pass, vec2 uv){
  ivec2 size = textureSize(previous_pass, 0).xy;
  ivec2 puv = ivec2(floor(uv*size/intensity)*intensity);
  return texelFetch(previous_pass, puv, 0);
}
