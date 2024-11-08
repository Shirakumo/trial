#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float intensity = 1.0;

void main(){
  ivec2 size = textureSize(previous_pass, 0).xy;
  ivec2 puv = ivec2(floor(uv*size/intensity)*intensity);
  color = texelFetch(previous_pass, puv, 0);
}
