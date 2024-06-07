#section FRAGMENT_SHADER
uniform float offset = 3.0;
uniform sampler2D previous_pass;
in vec2 uv;
out vec4 color;

void main(){
  vec2 r_offset = vec2(        0,  offset/sqrt(3));
  vec2 g_offset = vec2(-offset/2, -offset/(2*sqrt(3)));
  vec2 b_offset = vec2( offset/2, -offset/(2*sqrt(3)));
  vec2 size = textureSize(previous_pass, 0);
  vec4 _r = texture(previous_pass, uv+(r_offset/size));
  vec4 _g = texture(previous_pass, uv+(g_offset/size));
  vec4 _b = texture(previous_pass, uv+(b_offset/size));
  color = vec4(_r.r, _g.g, _b.b, texture(previous_pass, uv).a);
}
