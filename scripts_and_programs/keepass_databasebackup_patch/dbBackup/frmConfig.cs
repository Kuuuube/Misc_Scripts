// --------------------------------------------------------------------------------------------------------------------
// <copyright file="frmConfig.cs" company="">
//   
// </copyright>
// <summary>
//   The frm config.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace DataBaseBackup
{
    using System;
    using System.Collections.Generic;
    using System.Collections.Specialized;
    using System.Diagnostics;
    using System.IO;
    using System.Windows.Forms;

    using DataBaseBackup.Properties;

    using KeePass.UI;
    using KeePass.App.Configuration;

    /// <summary>
    /// The frm config.
    /// </summary>
    public partial class frmConfig : Form
    {
        #region Constructors and Destructors

        /// <summary>
        /// Initializes a new instance of the <see cref="frmConfig"/> class.
        /// </summary>
        public frmConfig(AppConfig appConfig)
        {
            this._appConfig = appConfig;

            this.InitializeComponent();
        }

        #endregion

        #region Methods

        /// <summary>
        /// function de validation avant enregistrement
        /// </summary>
        /// <returns>
        /// The <see cref="bool"/>.
        /// </returns>
        private bool _Valid()
        {
            return true;
        }

        private AppConfig _appConfig;

        /// <summary>
        /// The btn add_ click.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void btnAdd_Click(object sender, EventArgs e)
        {
            if (this.txtDestination.Text != string.Empty && Directory.Exists(this.txtDestination.Text))
            {
                this.lbFolder.Items.Add(this.txtDestination.Text);
                this.txtDestination.Text = string.Empty;
            }
        }

        /// <summary>
        /// The btn browse_ click.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void btnBrowse_Click(object sender, EventArgs e)
        {
            if (this.fbdBrowse.ShowDialog() == DialogResult.OK)
            {
                this.txtDestination.Text = this.fbdBrowse.SelectedPath;
            }
        }

        /// <summary>
        /// The btn cancel_ click.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void btnCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        /// <summary>
        /// The btn help date format_ click.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void btnHelpDateFormat_Click(object sender, EventArgs e)
        {
            Process.Start("http://msdn.microsoft.com/en-us/library/8kb3ddd4.aspx");
        }

        /// <summary>
        /// The btn o k_ click.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void btnOK_Click(object sender, EventArgs e)
        {
            if (this._Valid())
            {
                this._appConfig.HistoQty = (uint)this.txtQtyBackup.Value;
                if (this._appConfig.HistoFolder == null)
                {
                    this._appConfig.HistoFolder = new StringCollection();
                }

                var tempHistoFolder = new StringCollection();
                foreach (string it in this.lbFolder.Items)
                {
                    tempHistoFolder.Add(it);
                }

                this._appConfig.HistoFolder = tempHistoFolder;
                this._appConfig.DateFormat = this.txtDateFormat.Text;
                this.Close();
            }
        }

        /// <summary>
        /// The btn remove_ click.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void btnRemove_Click(object sender, EventArgs e)
        {
            var SelectItem = new List<string>();

            foreach (string  it in this.lbFolder.SelectedItems)
            {
                SelectItem.Add(it);
            }

            foreach (string it in SelectItem)
            {
                this.lbFolder.Items.Remove(it);
            }
        }

        /// <summary>
        /// The frm config_ load.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void frmConfig_Load(object sender, EventArgs e)
        {
            // banner
            this.picBannerImage.Image = BannerFactory.CreateBanner(
                this.picBannerImage.Width, 
                this.picBannerImage.Height, 
                BannerStyle.Default, 
                Resources.hd2_backup48x48, 
                "Configuration", 
                "Configure option for DBBackup");
            this.Icon = Resources.hd2_backup;

            this.txtQtyBackup.Value = this._appConfig.HistoQty;
            if (this._appConfig.HistoFolder != null)
            {
                foreach (string it in this._appConfig.HistoFolder)
                {
                    if (it != null)
                    {
                        this.lbFolder.Items.Add(it);
                    }
                }
            }

            this.txtDateFormat.Text = this._appConfig.DateFormat;
        }

        /// <summary>
        /// The lb folder_ selected index changed.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void lbFolder_SelectedIndexChanged(object sender, EventArgs e)
        {
            this.toolTip1.SetToolTip(this.lbFolder, string.Empty);
            if (this.lbFolder.SelectedItem != null)
            {
                this.toolTip1.SetToolTip(this.lbFolder, this.lbFolder.SelectedItem.ToString());
            }
        }

        #endregion
    }
}