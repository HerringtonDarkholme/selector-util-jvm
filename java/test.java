import h.chan.selector.Selector;
import h.chan.selector.SelectorsOpt;

class JavaTest {
    public static void main(String[] args) {
        SelectorsOpt selector = Selector.apply("#main");
        System.out.print(selector.contains("div#main"));
    }
}

