in vec2 tex_coord;
out vec4 color;
uniform sampler2D previous_pass;
uniform float intensity = 1.0;

void main(){
  ivec2 size = textureSize(previous_pass, 0);
  float blurSizeH = intensity / size.x;
  float blurSizeV = intensity / size.y;
  vec4 sum = vec4(0.0);
  for (int x=-4; x<=4; x++){
    for (int y=-4; y<=4; y++){
      sum += texture(previous_pass,
                     vec2(tex_coord.x + x*blurSizeH, tex_coord.y + y*blurSizeV)) / 81.0;
    }
  }
  color = sum;
}
