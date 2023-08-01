// --------------------------------------------------------------------------------------------------------------------
// <copyright file="DataBaseBackupExt.cs" company="">
//   
// </copyright>
// <summary>
//   This is the main plugin class. It must be named exactly
//   like the namespace and must be derived from
//   <c>KeePassPlugin</c>.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

namespace DataBaseBackup
{
    using System;
    using System.Diagnostics;
    using System.IO;
    using System.Net;
    using System.Net.Cache;
    using System.Text.RegularExpressions;
    using System.Windows.Forms;

    using DataBaseBackup.Properties;

    using KeePass.Forms;
    using KeePass.Plugins;

    /// <summary>
    ///     This is the main plugin class. It must be named exactly
    ///     like the namespace and must be derived from
    ///     <c>KeePassPlugin</c>.
    /// </summary>
    public sealed class DataBaseBackupExt : Plugin
    {
        // The sample plugin remembers its host in this variable.
        #region Fields

        /// <summary>
        /// The m_host.
        /// </summary>
        private IPluginHost m_host;

        private AppConfig m_appConfig;

        /// <summary>
        /// The m_ts separator.
        /// </summary>
        private ToolStripSeparator m_tsSeparator;

        /// <summary>
        /// The m_tsmi automatic backup.
        /// </summary>
        private ToolStripMenuItem m_tsmiAutomaticBackup;

        /// <summary>
        /// The m_tsmi backup now.
        /// </summary>
        private ToolStripMenuItem m_tsmiBackupNow;

        /// <summary>
        /// The m_tsmi config.
        /// </summary>
        private ToolStripMenuItem m_tsmiConfig;

        /// <summary>
        /// The m_tsmi popup.
        /// </summary>
        private ToolStripMenuItem m_tsmiPopup;

        #endregion

        #region Public Methods and Operators

        /// <summary>
        /// The <c>Initialize</c> function is called by KeePass when
        ///     you should initialize your plugin (create menu items, etc.).
        /// </summary>
        /// <param name="host">
        /// Plugin host interface. By using this
        ///     interface, you can access the KeePass main window and the
        ///     currently opened database.
        /// </param>
        /// <returns>
        /// You must return <c>true</c> in order to signal
        ///     successful initialization. If you return <c>false</c>,
        ///     KeePass unloads your plugin (without calling the
        ///     <c>Terminate</c> function of your plugin).
        /// </returns>
        public override bool Initialize(IPluginHost host)
        {
            Debug.Assert(host != null);
            if (host == null)
            {
                return false;
            }

            this.m_host = host;

            this.m_appConfig = new AppConfig(this.m_host.CustomConfig);

            // Get a reference to the 'Tools' menu item container
            ToolStripItemCollection tsMenu = this.m_host.MainWindow.ToolsMenu.DropDownItems;

            // Add a separator at the bottom
            this.m_tsSeparator = new ToolStripSeparator();
            tsMenu.Add(this.m_tsSeparator);

            // Add the popup menu item
            this.m_tsmiPopup = new ToolStripMenuItem();
            this.m_tsmiPopup.Text = "DB Backup plug-in";
            tsMenu.Add(this.m_tsmiPopup);

            // Add menu item 'Backup now'
            this.m_tsmiBackupNow = new ToolStripMenuItem();
            this.m_tsmiBackupNow.Text = "Backup DB NOW !";
            this.m_tsmiBackupNow.Click += this.OnMenuBackupNow;
            this.m_tsmiBackupNow.Enabled = true;
            this.m_tsmiPopup.DropDownItems.Add(this.m_tsmiBackupNow);

            // Add a separator
            this.m_tsSeparator = new ToolStripSeparator();
            this.m_tsmiPopup.DropDownItems.Add(this.m_tsSeparator);

            // Add menu item 'Backup now'
            this.m_tsmiAutomaticBackup = new ToolStripMenuItem();
            this.m_tsmiAutomaticBackup.Text = "Automatically Backup DB";
            this.m_tsmiAutomaticBackup.Checked = this.m_appConfig.AutoBackup;
            this.m_tsmiAutomaticBackup.Click += this.OnMenuAutomaticBackup;
            this.m_tsmiAutomaticBackup.Enabled = true;
            this.m_tsmiPopup.DropDownItems.Add(this.m_tsmiAutomaticBackup);

            // Add menu item 'Backup now'
            this.m_tsmiConfig = new ToolStripMenuItem();
            this.m_tsmiConfig.Text = "Configure";
            this.m_tsmiConfig.Click += this.OnMenuConfig;
            this.m_tsmiConfig.Enabled = true;
            this.m_tsmiPopup.DropDownItems.Add(this.m_tsmiConfig);

            // We want a notification when the user tried to save the
            // current database
            this.m_host.MainWindow.FileSaved += this.OnFileSaved;

            return true; // Initialization successful
        }

        /// <summary>
        ///     The <c>Terminate</c> function is called by KeePass when
        ///     you should free all resources, close open files/streams,
        ///     etc. It is also recommended that you remove all your
        ///     plugin menu items from the KeePass menu.
        /// </summary>
        public override void Terminate()
        {
            // Remove all of our menu items
            ToolStripItemCollection tsMenu = this.m_host.MainWindow.ToolsMenu.DropDownItems;
            tsMenu.Remove(this.m_tsSeparator);
            tsMenu.Remove(this.m_tsmiPopup);
            tsMenu.Remove(this.m_tsmiBackupNow);
            tsMenu.Remove(this.m_tsmiAutomaticBackup);
            tsMenu.Remove(this.m_tsmiConfig);

            // Important! Remove event handlers!
            this.m_host.MainWindow.FileSaved -= this.OnFileSaved;
        }

        #endregion

        #region Methods

        /// <summary>
        /// The on file saved.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void OnFileSaved(object sender, FileSavedEventArgs e)
        {
            if (this.m_appConfig.AutoBackup)
            {
                this._BackupDB();
            }
        }

        /// <summary>
        /// The on menu automatic backup.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void OnMenuAutomaticBackup(object sender, EventArgs e)
        {
            this.m_appConfig.AutoBackup = !this.m_appConfig.AutoBackup;
            ((ToolStripMenuItem)sender).Checked = this.m_appConfig.AutoBackup;
        }

        /// <summary>
        /// The on menu backup now.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void OnMenuBackupNow(object sender, EventArgs e)
        {
            this._BackupDB();
        }

        /// <summary>
        /// The on menu config.
        /// </summary>
        /// <param name="sender">
        /// The sender.
        /// </param>
        /// <param name="e">
        /// The e.
        /// </param>
        private void OnMenuConfig(object sender, EventArgs e)
        {
            var frm = new frmConfig(this.m_appConfig);
            frm.ShowDialog();
            frm.Dispose();
            frm = null;
        }

        /// <summary>
        ///     Backup db to all configure directory
        /// </summary>
        private void _BackupDB()
        {
            if (this.m_host.Database.IsOpen)
            {
                string SourceFile = string.Empty;
                string SourceFileName = string.Empty;
                string BackupFile = string.Empty;
                SourceFileName = Helper.GetSourceFileName(this.m_host);

                if (this.m_host.Database.IOConnectionInfo.IsLocalFile())
                {
                    SourceFile = this.m_host.Database.IOConnectionInfo.Path;
                }
                else
                {
                    // remote file
                    SourceFile = Path.GetTempFileName();

                    var wc = new WebClient();

                    wc.CachePolicy = new RequestCachePolicy(RequestCacheLevel.NoCacheNoStore);

                    if ((this.m_host.Database.IOConnectionInfo.UserName.Length > 0) || (this.m_host.Database.IOConnectionInfo.Password.Length > 0))
                    {
                        wc.Credentials = new NetworkCredential(this.m_host.Database.IOConnectionInfo.UserName, this.m_host.Database.IOConnectionInfo.Password);
                    }

                    wc.DownloadFile(this.m_host.Database.IOConnectionInfo.Path, SourceFile);
                    wc.Dispose();
                    wc = null;
                }

                var tempHistoFolder=this.m_appConfig.HistoFolder;
                if (tempHistoFolder != null)
                {
                    foreach (string it in tempHistoFolder)
                    {
                        // read log file
                        string BackupLogFile = it + "/" + Helper.GetLogFileName(this.m_host);
                        string[] LogFile = null;
                        if (File.Exists(BackupLogFile))
                        {
                            LogFile = File.ReadAllLines(BackupLogFile);
                        }

                        if (Directory.Exists(it))
                        {
                            // create file
                            BackupFile = it + "/" + SourceFileName + "_" + DateTime.Now.ToString(this.m_appConfig.DateFormat) + ".kdbx";
                            File.Copy(SourceFile, BackupFile,true);
                            
                            // delete extra file
                            if (LogFile != null)
                            {
                                if (LogFile.Length + 1 > (int)this.m_appConfig.HistoQty)
                                {
                                    for (int LoopDelete = (int)this.m_appConfig.HistoQty - 1; LoopDelete < LogFile.Length; LoopDelete++)
                                    {
                                        if (File.Exists(LogFile[LoopDelete]))
                                        {
                                            File.Delete(LogFile[LoopDelete]);
                                        }
                                    }
                                }
                            }

                            // write log file
                            TextWriter fLog = new StreamWriter(BackupLogFile, false);
                            fLog.WriteLine(BackupFile);
                            if (LogFile != null)
                            {
                                var LoopMax = (uint)LogFile.Length;
                                if (LoopMax > this.m_appConfig.HistoQty)
                                {
                                    LoopMax = (uint)this.m_appConfig.HistoQty;
                                }

                                for (uint i = 0; i < LoopMax; i++)
                                {
                                    fLog.WriteLine(LogFile[i]);
                                }
                            }

                            fLog.Close();
                            fLog.Dispose();
                            fLog = null;
                        }
                        else
                        {
                            MessageBox.Show("Folder not found  : " + it);
                        }
                    }
                }

                // delete temp remote file
                if (!this.m_host.Database.IOConnectionInfo.IsLocalFile())
                {
                    File.Delete(SourceFile);
                }
            }
            else
            {
                MessageBox.Show("Database is not open.");
            }
        }

        /// <summary>
        /// The _ remove special chars.
        /// </summary>
        /// <param name="input">
        /// The input.
        /// </param>
        /// <returns>
        /// The <see cref="string"/>.
        /// </returns>
        private string _RemoveSpecialChars(string input)
        {
            return Regex.Replace(input, @"[^0-9a-zA-Z\._]", string.Empty);
        }

        #endregion
    }
}