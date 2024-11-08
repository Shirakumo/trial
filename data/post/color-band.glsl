#section FRAGMENT_SHADER
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float bands = 1.0;

void main(){
  color = texture(previous_pass, uv);
  color.rgb = floor(color.rgb*bands)/bands;
}
