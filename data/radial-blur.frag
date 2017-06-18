uniform sampler2D previous_pass;
uniform float strength = 0.2;
uniform float exposure = 0.8;
uniform vec2 origin = vec2(0.5, 0.5);

in vec2 tex_coord;
out vec4 color;

void main(void){
  vec2 size = 1 / textureSize(previous_pass, 0);
  vec4 color_sum = vec4(0.0, 0.0, 0.0, 0.0);
  vec2 _tex_coord = tex_coord + size * 0.5 - origin;

  for (int i = 0; i < 12; i++) {
    float scale = 1.0 - strength * (float(i) / 11.0);
    color_sum += texture(previous_pass, _tex_coord * scale + origin);
  }

  color = color_sum / 12.0 * exposure;
}
