int * global_ptr;

struct bar {
  int * bar_field;
  struct {
    int * oh_noes;
  } * x;
} * zoom;

int * func(int * param_ptr)
{
  int * local_ptr = 0;

  {
    int * sub_scope_ptr = 0;
  }

  int * foo(int * foo_param, int * foo_param2)
  {
    int * local_foo,
        * local_foo_2;
    {
      int * sub_foo_param = 0;
    }

    return *local_foo;
  }

  int i = 0;

  for( i = 0; i < 10; i++)
  {
    int * for_ptr;
  }

  while(i++ < 100)
  {
    int * while_ptr;
  }

  return local_ptr;
}
