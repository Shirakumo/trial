// We need to include it as a uniform, but the struct include
// would not create the correct uniform block.
// #include trial::morph-data
#define SIMULTANEOUS_MORPHS 8
layout(std140) uniform MorphData {
  int count;
  float weights[SIMULTANEOUS_MORPHS];
  int indices[SIMULTANEOUS_MORPHS];
} morph_data;

#extension GL_ARB_shader_storage_buffer_object : require
#extension GL_ARB_uniform_buffer_std430_layout : require
struct morph_vert{
  vec3 position;
  vec3 normal;
  vec2 uv;
};

layout(std430) uniform MorphTargets {
  struct morph_vert data[][];
} morph_targets;

void morph_vertex(inout vec3 position, inout vec3 normal, inout vec2 uv){
  for(int i=0; i<min(morph_data.count, SIMULTANEOUS_MORPHS); ++i){
    int index = morph_data.indices[i]*3;
    float weight = morph_data.weights[i];
    // NOTE: the layout here is... very suboptimal. We would *much* rather store the
    //       data with the vertices being the layers so that all the morph data is nice
    //       and adjacent, but the depht/layer count is heavily restricted compared to
    //       the width and height, so we can't do that. SAD!
    struct morph_vert data = morph_targets[gl_VertexID][index];
    position += weight * data.position;
    normal += weight * data.normal;
    uv += weight * data.uv;
  }
}
