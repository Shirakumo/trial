in vec2 texCoord;
out vec4 color;
uniform sampler2D previousPass;
uniform float intensity = 1.0;

void main(){
  ivec2 size = textureSize(previousPass, 0);
  float blurSizeH = intensity / size.x;
  float blurSizeV = intensity / size.y;
  vec4 sum = vec4(0.0);
  for (int x=-4; x<=4; x++){
    for (int y=-4; y<=4; y++){
      sum += texture(previousPass,
                     vec2(texCoord.x + x*blurSizeH, texCoord.y + y*blurSizeV)) / 81.0;
    }
  }
  color = sum;
}
