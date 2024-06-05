#section VERTEX_SHADER
#include "skin-matrix.glsl"
#include "morph.glsl"

layout (location = 0) in vec3 in_position;
layout (location = 3) in vec4 in_joints;
layout (location = 4) in vec4 in_weights;
uniform mat4 model_matrix;
uniform int shadow_map_id;
uniform int animated;
uniform int morphed;

void main(){
  vec3 position = in_position;
  vec3 normal = vec3(0);
  vec2 uv = vec2(0);

  if(animated){
    morph_vertex(position, normal, uv);
    skin_vertex(position, normal, in_joints, in_weights);
  }
  
  vec4 world_position = model_matrix * vec4(position, 1);
  gl_Position = shadow_info[shadow_map_id].projection_matrix * world_position;
}

#section FRAGMENT_SHADER
void main(){}
