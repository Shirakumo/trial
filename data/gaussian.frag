in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;
uniform float intensity = 2.0;
uniform vec2 dir = vec2(1.0, 0.0);

void main() {
  ivec2 size = textureSize(previousPass, 0);
  // FIXME: Calculate the dimension properly
  float blur = intensity;
  if(dir.x == 1.0)
    blur /= size.x;
  else
    blur /= size.y;
  vec2 tc = texCoord;
  color = vec4(0.0);

  color += texture2D(previousPass, vec2(tc.x - 4.0*blur*dir.x, tc.y - 4.0*blur*dir.y)) * 0.0162162162;
  color += texture2D(previousPass, vec2(tc.x - 3.0*blur*dir.x, tc.y - 3.0*blur*dir.y)) * 0.0540540541;
  color += texture2D(previousPass, vec2(tc.x - 2.0*blur*dir.x, tc.y - 2.0*blur*dir.y)) * 0.1216216216;
  color += texture2D(previousPass, vec2(tc.x - 1.0*blur*dir.x, tc.y - 1.0*blur*dir.y)) * 0.1945945946;

  color += texture2D(previousPass, vec2(tc.x, tc.y)) * 0.2270270270;

  color += texture2D(previousPass, vec2(tc.x + 1.0*blur*dir.x, tc.y + 1.0*blur*dir.y)) * 0.1945945946;
  color += texture2D(previousPass, vec2(tc.x + 2.0*blur*dir.x, tc.y + 2.0*blur*dir.y)) * 0.1216216216;
  color += texture2D(previousPass, vec2(tc.x + 3.0*blur*dir.x, tc.y + 3.0*blur*dir.y)) * 0.0540540541;
  color += texture2D(previousPass, vec2(tc.x + 4.0*blur*dir.x, tc.y + 4.0*blur*dir.y)) * 0.0162162162;
}
