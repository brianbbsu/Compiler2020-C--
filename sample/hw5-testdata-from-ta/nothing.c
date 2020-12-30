void Nothing1() {
  ;;;;;;;;;
  write("done\n");
  ;;;;;;;;;
}

void Nothing2() {
  {
    {
      ;;
    }
    ;
    {
      {
        ;;;
      }
    }
    write("done\n");
  }
}

void Nothing3() {
  int a;
  a = 0;
  write("done\n");
}
void Nothing4() {
  int a[123];
  write("done\n");
}

int main() {
  Nothing1();
  Nothing2();
  Nothing3();
  Nothing4();
}
