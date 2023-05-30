#section VERTEX_SHADER
layout (location = 0) in vec3 in_position;
uniform mat4 model_matrix;
uniform int shadow_map_id;

void main(){
  vec4 world_position = model_matrix * vec4(in_position, 1);
  gl_Position = shadow_info[shadow_map_id].projection_matrix * world_position;
}

#section FRAGMENT_SHADER
void main(){}
