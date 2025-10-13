using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace GUI1
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            HelpWindow hwin = new HelpWindow();
            hwin.Show();
        }

        private void SendText_Button_Click(object sender, RoutedEventArgs e)
        {
            string input_txt = Input_Text.Text;

// Add the f# stub for the interpreter
        }
    }
}