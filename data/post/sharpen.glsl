#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float intensity = 1.0;

void main(){
  float n = -intensity;
  float c = intensity * 4 + 1;

  color
    = n * textureOffset(previous_pass, uv, ivec2(0, +1))
    + n * textureOffset(previous_pass, uv, ivec2(+1, 0))
    + c * texture(previous_pass, uv)
    + n * textureOffset(previous_pass, uv, ivec2(0, -1))
    + n * textureOffset(previous_pass, uv, ivec2(-1, 0));
}
