uniform samplerCube cubemap;

void main(){
    gl_FragColor = textureCube(cubemap, gl_TexCoord[0]);
}
