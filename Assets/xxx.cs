using System.Collections;
using System.Collections.Generic;
using UnityEngine;
[ExecuteInEditMode]
public class xxx : MonoBehaviour
{
    public string Name02 = "SSS";
    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        Vector3 pos = transform.position;
        Shader.SetGlobalVector(Name02, pos);
    }
}
