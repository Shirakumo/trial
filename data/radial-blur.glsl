#section FRAGMENT_SHADER
uniform sampler2D previous_pass;
uniform float intensity = 0.2;
uniform float exposure = 0.8;
uniform vec2 origin = vec2(0.5, 0.5);
uniform int samples = 12;

in vec2 uv;
out vec4 color;

void main(void){
  vec2 size = 1 / textureSize(previous_pass, 0);
  vec4 color_sum = vec4(0.0, 0.0, 0.0, 0.0);
  vec2 _uv = uv + size * 0.5 - origin;

  for (int i = 0; i < samples; i++) {
    float scale = 1.0 - intensity * (float(i) / samples);
    color_sum += texture(previous_pass, _uv * scale + origin);
  }

  color = color_sum / samples * exposure;
}
