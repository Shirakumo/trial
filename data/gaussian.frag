in vec2 tex_coord;
out vec4 color;
uniform sampler2D previous_pass;
uniform float intensity = 2.0;
uniform vec2 dir = vec2(1.0, 0.0);
// FIXME: glsl-toolkit failure
//uniform float weight[5] = float[] (0.227027, 0.1945946, 0.1216216, 0.054054, 0.016216);

void main(){
  vec2 tex_offset = 1.0 / textureSize(previous_pass, 0); // gets size of single texel
  vec3 result = texture(image, tex_coord).rgb * weight[0]; // current fragment's contribution
  if(dir.x == 1.0){
    for(int i = 1; i < 5; ++i){
      result += texture(image, tex_coord + vec2(tex_offset.x * i, 0.0)).rgb * weight[i];
      result += texture(image, tex_coord - vec2(tex_offset.x * i, 0.0)).rgb * weight[i];
    }
  }else{
    for(int i = 1; i < 5; ++i){
      result += texture(image, tex_coord + vec2(0.0, tex_offset.y * i)).rgb * weight[i];
      result += texture(image, tex_coord - vec2(0.0, tex_offset.y * i)).rgb * weight[i];
    }
  }
  color = vec4(result, 1.0);
}
