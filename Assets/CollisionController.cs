using UnityEngine;

public class CollisionController : MonoBehaviour
{
    public GameObject targetObject;

    void OnCollisionEnter(Collision collision)
    {
        // 碰撞发生时激活目标物体
        if (collision.gameObject == targetObject)
        {
            targetObject.SetActive(true);
        }
    }

    void OnCollisionExit(Collision collision)
    {
        // 碰撞结束时关闭目标物体
        if (collision.gameObject == targetObject)
        {
            targetObject.SetActive(false);
        }
    }
}
